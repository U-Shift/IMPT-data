library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# ----------------------------
# 1) Ler freguesias (2024) e acidentes (IG)
# ----------------------------
freguesias <- impt_read("/geo/freguesias_2024_unique.gpkg")

acidentes_path <- impt_read(URLencode("/safety/13-SegurançaRodoviária.gpkg"))
# stopifnot(file.exists(path.expand(acidentes_path)))

ig <- st_read(
  dsn = acidentes_path,
  layer = "ANSR_AML_sinistralidade_02 - IG",
  quiet = TRUE
)
table(ig$Concelho, ig$Ano) # praticamente igual à TML, diferença de 3
# table(is.na(ig$Concelho)) #0

# ----------------------------
# 2) Join espacial (pontos -> freguesia nova)
# ----------------------------
ig_freg <- st_join(
  ig |>
    select(-Freguesia) |>
    st_transform(st_crs(freguesias)),
  freguesias %>% select(dtmnfr, freguesia)
)

table(is.na(ig_freg$freguesia)) # 173
# mapview::mapview(ig_freg) + mapview::mapview(freguesias, alpha.regions = 0.2)

ig_freg <- ig_freg |> filter(!is.na(freguesia)) # remove os 173 sem freguesia (pontes!) (que são poucos, e não vale a pena resolver por nearest)

# only last 5 years
table(ig_freg$Ano)
ig_freg <- ig_freg |> filter(Ano %in% c(2019:2023))

# select only the variables that matter
names(ig_freg)
ig_redux <- ig_freg |> select(
  dtmnfr, freguesia, Ano, VM.30d, X_VM.30d, FG.30d, X_FG.30d, FL.30d, X_FL.30d, X_Veículo,
  X_Veícul_1, X_Ciclomoto, X_Desconhec, X_Máquina, X_Motociclo, X_Motocic_1, X_Quadricic, X_Triciclo,
  X_Veícul_2, X_Veícul_3, X_Veícul_4, X_Velocípe, X_Velocí_1, X_nd, X_Total.Ger, Total.Peõ,
  X_IG, Localizaç, Luminosida
)
accidents <- ig_redux |>
  mutate(
    vitimas_mortais30 = as.integer(X_VM.30d),
    feridos_graves30 = as.integer(X_FG.30d),
    feridos_ligeiros30 = as.integer(X_FL.30d),
    total_vitimas30 = vitimas_mortais30 + feridos_graves30 + feridos_ligeiros30,
    veh_motorizado = X_Veículo + X_Veícul_1 + X_Ciclomoto + X_Desconhec + X_Máquina + X_Motociclo + X_Motocic_1 + X_Quadricic + X_Triciclo + X_Veícul_2 + X_Veícul_3 + X_Veícul_4,
    veh_bicicleta = as.integer(X_Velocípe) + as.integer(X_Velocí_1),
    veh_peoes = as.integer(Total.Peõ),
    total_veh = veh_motorizado + veh_bicicleta + veh_peoes,
    localidades_dentro = case_when(Localizaç == "Dentro das localidades" ~ 1, TRUE ~ 0),
    # localidades_fora = case_when(Localizaç== "Fora das localidades" ~ 1, TRUE ~ 0),
    noite = case_when(grepl("Noite", Luminosida, ignore.case = TRUE) ~ 1, TRUE ~ 0)
  )
sum(accidents$vitimas_mortais30) # 433 mortos a 30 dias

# check with pmus data
# vitimas = accidents |>
#   st_drop_geometry() |>
#   group_by(Ano) |>
#   summarise(vitimas_mortais30 = sum(vitimas_mortais30, na.rm = TRUE),
#             feridos_graves30 = sum(feridos_graves30, na.rm = TRUE),
#             feridos_ligeiros30 = sum(feridos_ligeiros30, na.rm = TRUE),
#             total_vitimas30 = sum(total_vitimas30, na.rm = TRUE),
#             acidentes = n())
# veiculos = accidents |>
#   st_drop_geometry() |>
#   group_by(Ano) |>
#   summarise(veh_motorizado = sum(veh_motorizado, na.rm = TRUE),
#             veh_bicicleta = sum(veh_bicicleta, na.rm = TRUE),
#             veh_peoes = sum(veh_peoes, na.rm = TRUE),
#             total_veiculos = sum(total_veh, na.rm = TRUE),
#             acidentes = n())


accidents_redux <- accidents |>
  st_drop_geometry() |>
  ungroup() |>
  group_by(dtmnfr, freguesia) |>
  summarise(
    total_acidentes = n(), # total 5 anos
    localidades_dentro = sum(localidades_dentro),
    noite = sum(noite),
    vitimas_mortais30 = sum(vitimas_mortais30, na.rm = TRUE),
    feridos_graves30 = sum(feridos_graves30, na.rm = TRUE),
    feridos_ligeiros30 = sum(feridos_ligeiros30, na.rm = TRUE),
    total_vitimas30 = sum(total_vitimas30, na.rm = TRUE),
    veh_motorizado = sum(veh_motorizado, na.rm = TRUE),
    veh_bicicleta = sum(veh_bicicleta, na.rm = TRUE),
    veh_peoes = sum(veh_peoes, na.rm = TRUE),
    total_veiculos = sum(total_veh, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(freguesia)

# #conferir
# sum(accidents_redux$total_acidentes) # 41727
# nrow(accidents)
# sum(accidents$noite)
# sum(accidents_redux$noite) # 11235
# sum(accidents$total_vitimas30)
# sum(accidents_redux$total_vitimas30) # 51061


#### Populations and municipio
### Join with municipio - use ours!
### Join with population and municipio
census24_fregmun <- read.csv("useful_data/census24_fregmun.csv") |>
  mutate(
    freg_id = as.character(freg_id),
    mun_id = as.character(mun_id)
  )

accidents_redux <- accidents_redux |>
  left_join(census24_fregmun |> select(freg_id, mun_id, population), by = c("dtmnfr" = "freg_id")) |>
  rename(freg_id = dtmnfr)


# ==========================================================
# (5) ÍNDICE DE GRAVIDADE
# VM30 / (acidentes com vítimas: mortos + feridos)
# ==========================================================

accidents_redux <- accidents_redux |>
  mutate(
    indice_gravidade = vitimas_mortais30 / total_vitimas30,
    indice_gravidade_carro = ifelse(veh_motorizado > 0, vitimas_mortais30 / veh_motorizado, NA_real_), # ignora se não houve carro involvido
    indice_gravidade_bicicleta = ifelse(veh_bicicleta > 0, vitimas_mortais30 / veh_bicicleta, NA_real_),
    indice_gravidade_peoes = ifelse(veh_peoes > 0, vitimas_mortais30 / veh_peoes, NA_real_)
  )

# NA means that there were no vehicles of that type involved, so we can't calculate a severity index for that mode.

accidents_redux <- accidents_redux |>
  mutate(
    acidentes_per_1000res = total_acidentes / population * 1000,
    vitimas_mortais30_per_1000res = vitimas_mortais30 / population * 1000,
    localidades_dentro_perc = localidades_dentro / total_acidentes,
    noite_perc = noite / total_acidentes
  )


# apenas dentro das localidades -------------------------------------------

accidents_redux_localidades <- accidents |>
  st_drop_geometry() |>
  ungroup() |>
  filter(localidades_dentro == 1) |>
  group_by(dtmnfr, freguesia) |>
  summarise(
    total_acidentes = n(),
    noite = sum(noite),
    vitimas_mortais30 = sum(vitimas_mortais30, na.rm = TRUE),
    feridos_graves30 = sum(feridos_graves30, na.rm = TRUE),
    feridos_ligeiros30 = sum(feridos_ligeiros30, na.rm = TRUE),
    total_vitimas30 = sum(total_vitimas30, na.rm = TRUE),
    veh_motorizado = sum(veh_motorizado, na.rm = TRUE),
    veh_bicicleta = sum(veh_bicicleta, na.rm = TRUE),
    veh_peoes = sum(veh_peoes, na.rm = TRUE),
    total_veiculos = sum(total_veh, na.rm = TRUE)
  ) |>
  ungroup() |>
  left_join(census24_fregmun |> select(freg_id, mun_id, population), by = c("dtmnfr" = "freg_id")) |>
  rename(freg_id = dtmnfr) |>
  mutate(
    indice_gravidade = vitimas_mortais30 / total_vitimas30,
    indice_gravidade_carro = ifelse(veh_motorizado > 0, vitimas_mortais30 / veh_motorizado, NA_real_), # ignora se não houve carro involvido
    indice_gravidade_bicicleta = ifelse(veh_bicicleta > 0, vitimas_mortais30 / veh_bicicleta, NA_real_),
    indice_gravidade_peoes = ifelse(veh_peoes > 0, vitimas_mortais30 / veh_peoes, NA_real_)
  ) |>
  mutate(
    acidentes_per_1000res = total_acidentes / population * 1000,
    vitimas_mortais30_per_1000res = vitimas_mortais30 / population * 1000,
    noite_perc = noite / total_acidentes
  )


### Organizar
accidents_final <- accidents_redux |>
  select(
    freg_id, freguesia, mun_id, population, total_acidentes, localidades_dentro, localidades_dentro_perc, noite, noite_perc,
    vitimas_mortais30, feridos_graves30, feridos_ligeiros30, total_vitimas30,
    veh_motorizado, veh_bicicleta, veh_peoes, total_veiculos,
    indice_gravidade, indice_gravidade_carro, indice_gravidade_bicicleta, indice_gravidade_peoes,
    acidentes_per_1000res, vitimas_mortais30_per_1000res
  ) |>
  arrange(mun_id, freguesia)

accidents_localidades_final <- accidents_redux_localidades |>
  select(
    freg_id, freguesia, mun_id, population, total_acidentes, noite, noite_perc,
    vitimas_mortais30, feridos_graves30, feridos_ligeiros30, total_vitimas30,
    veh_motorizado, veh_bicicleta, veh_peoes, total_veiculos,
    indice_gravidade, indice_gravidade_carro, indice_gravidade_bicicleta, indice_gravidade_peoes,
    acidentes_per_1000res, vitimas_mortais30_per_1000res
  ) |>
  arrange(mun_id, freguesia)


# acidentes por município -------------------------------------------------

accidents_final_municipio <- accidents_final |>
  group_by(mun_id) |>
  summarize_at(
    vars(
      population, total_acidentes, localidades_dentro, noite,
      vitimas_mortais30, feridos_graves30, feridos_ligeiros30,
      total_vitimas30, veh_motorizado, veh_bicicleta,
      veh_peoes, total_veiculos
    ),
    sum,
    na.rm = TRUE
  ) |>
  mutate(
    localidades_dentro_perc = localidades_dentro / total_acidentes,
    noite_perc = noite / total_acidentes,
    vitimas_mortais30 / total_vitimas30,
    indice_gravidade = vitimas_mortais30 / total_vitimas30,
    indice_gravidade_carro = ifelse(veh_motorizado > 0, vitimas_mortais30 / veh_motorizado, NA_real_), # ignora se não houve carro involvido
    indice_gravidade_bicicleta = ifelse(veh_bicicleta > 0, vitimas_mortais30 / veh_bicicleta, NA_real_),
    indice_gravidade_peoes = ifelse(veh_peoes > 0, vitimas_mortais30 / veh_peoes, NA_real_),
    acidentes_per_1000res = total_acidentes / population * 1000,
    vitimas_mortais30_per_1000res = vitimas_mortais30 / population * 1000
  ) |>
  select(
    mun_id, population, total_acidentes, localidades_dentro, localidades_dentro_perc, noite, noite_perc,
    vitimas_mortais30, feridos_graves30, feridos_ligeiros30, total_vitimas30,
    veh_motorizado, veh_bicicleta, veh_peoes, total_veiculos,
    indice_gravidade, indice_gravidade_carro, indice_gravidade_bicicleta, indice_gravidade_peoes,
    acidentes_per_1000res, vitimas_mortais30_per_1000res
  ) |>
  arrange(mun_id)

sum(accidents_final_municipio$total_acidentes) # 41727
sum(accidents_final$total_acidentes) # 41727


accidents_final_municipio_localidades <- accidents_localidades_final |>
  group_by(mun_id) |>
  summarize_at(
    vars(
      population, total_acidentes, noite,
      vitimas_mortais30, feridos_graves30, feridos_ligeiros30,
      total_vitimas30, veh_motorizado, veh_bicicleta,
      veh_peoes, total_veiculos
    ),
    sum,
    na.rm = TRUE
  ) |>
  mutate(
    noite_perc = noite / total_acidentes,
    vitimas_mortais30 / total_vitimas30,
    indice_gravidade = vitimas_mortais30 / total_vitimas30,
    indice_gravidade_carro = ifelse(veh_motorizado > 0, vitimas_mortais30 / veh_motorizado, NA_real_), # ignora se não houve carro involvido
    indice_gravidade_bicicleta = ifelse(veh_bicicleta > 0, vitimas_mortais30 / veh_bicicleta, NA_real_),
    indice_gravidade_peoes = ifelse(veh_peoes > 0, vitimas_mortais30 / veh_peoes, NA_real_),
    acidentes_per_1000res = total_acidentes / population * 1000,
    vitimas_mortais30_per_1000res = vitimas_mortais30 / population * 1000
  ) |>
  select(
    mun_id, population, total_acidentes, noite, noite_perc,
    vitimas_mortais30, feridos_graves30, feridos_ligeiros30, total_vitimas30,
    veh_motorizado, veh_bicicleta, veh_peoes, total_veiculos,
    indice_gravidade, indice_gravidade_carro, indice_gravidade_bicicleta, indice_gravidade_peoes,
    acidentes_per_1000res, vitimas_mortais30_per_1000res
  ) |>
  arrange(mun_id)

sum(accidents_final_municipio_localidades$total_acidentes) # 35344
sum(accidents_localidades_final$total_acidentes) # 35344


# acidentes por grid -------------------------------------------------
grid <- impt_read("/geo/grelha_h3_r8.gpkg", quiet = TRUE)

ig_grid <- st_join(
  ig |>
    select(-Freguesia) |>
    st_transform(st_crs(grid)),
  grid
)

table(is.na(ig_grid$id)) # 851
# mapview::mapview(ig_grid) + mapview::mapview(grid, alpha.regions = 0.2) # há imensos na marginal e pontes que não são apanhados pela grid
ig_grid <- ig_grid |> filter(!is.na(id)) # remove os 173 sem freguesia (pontes!) (que são poucos, e não vale a pena resolver por nearest)

# only last 5 years
table(ig_grid$Ano)
ig_grid <- ig_grid |> filter(Ano %in% c(2019:2023))

# select only the variables that matter
names(ig_grid)
ig_grid_redux <- ig_grid |>
  select(
    id, Ano, VM.30d, X_VM.30d, FG.30d, X_FG.30d, FL.30d, X_FL.30d, X_Veículo,
    X_Veícul_1, X_Ciclomoto, X_Desconhec, X_Máquina, X_Motociclo, X_Motocic_1, X_Quadricic, X_Triciclo,
    X_Veícul_2, X_Veícul_3, X_Veícul_4, X_Velocípe, X_Velocí_1, X_nd, X_Total.Ger, Total.Peõ,
    X_IG, Localizaç, Luminosida
  ) |>
  rename(grid_id = id)
accidents_grid <- ig_grid_redux |>
  mutate(
    vitimas_mortais30 = as.integer(X_VM.30d),
    feridos_graves30 = as.integer(X_FG.30d),
    feridos_ligeiros30 = as.integer(X_FL.30d),
    total_vitimas30 = vitimas_mortais30 + feridos_graves30 + feridos_ligeiros30,
    veh_motorizado = X_Veículo + X_Veícul_1 + X_Ciclomoto + X_Desconhec + X_Máquina + X_Motociclo + X_Motocic_1 + X_Quadricic + X_Triciclo + X_Veícul_2 + X_Veícul_3 + X_Veícul_4,
    veh_bicicleta = as.integer(X_Velocípe) + as.integer(X_Velocí_1),
    veh_peoes = as.integer(Total.Peõ),
    total_veh = veh_motorizado + veh_bicicleta + veh_peoes,
    localidades_dentro = case_when(Localizaç == "Dentro das localidades" ~ 1, TRUE ~ 0),
    # localidades_fora = case_when(Localizaç== "Fora das localidades" ~ 1, TRUE ~ 0),
    noite = case_when(grepl("Noite", Luminosida, ignore.case = TRUE) ~ 1, TRUE ~ 0)
  )
sum(accidents_grid$vitimas_mortais30) # 415 mortos a 30 dias (menos 15 que nas feguesias)


accidents_grid_redux <- accidents_grid |>
  st_drop_geometry() |>
  ungroup() |>
  group_by(grid_id) |>
  summarise(
    total_acidentes = n(), # total 5 anos
    localidades_dentro = sum(localidades_dentro),
    noite = sum(noite),
    vitimas_mortais30 = sum(vitimas_mortais30, na.rm = TRUE),
    feridos_graves30 = sum(feridos_graves30, na.rm = TRUE),
    feridos_ligeiros30 = sum(feridos_ligeiros30, na.rm = TRUE),
    total_vitimas30 = sum(total_vitimas30, na.rm = TRUE),
    veh_motorizado = sum(veh_motorizado, na.rm = TRUE),
    veh_bicicleta = sum(veh_bicicleta, na.rm = TRUE),
    veh_peoes = sum(veh_peoes, na.rm = TRUE),
    total_veiculos = sum(total_veh, na.rm = TRUE)
  ) |>
  ungroup()


landuse_grid <- impt_read("/landuse/grid_with_cos.csv")
accidents_grid_redux <- accidents_grid_redux |>
  left_join(landuse_grid |> select(id, population), by = c("grid_id" = "id")) |>
  mutate(
    indice_gravidade = vitimas_mortais30 / total_vitimas30,
    indice_gravidade_carro = ifelse(veh_motorizado > 0, vitimas_mortais30 / veh_motorizado, NA_real_), # ignora se não houve carro involvido
    indice_gravidade_bicicleta = ifelse(veh_bicicleta > 0, vitimas_mortais30 / veh_bicicleta, NA_real_),
    indice_gravidade_peoes = ifelse(veh_peoes > 0, vitimas_mortais30 / veh_peoes, NA_real_)
  )

# NA means that there were no vehicles of that type involved, so we can't calculate a severity index for that mode.

accidents_grid_redux <- accidents_grid_redux |>
  mutate(
    acidentes_per_1000res = total_acidentes / population * 1000,
    vitimas_mortais30_per_1000res = vitimas_mortais30 / population * 1000,
    localidades_dentro_perc = localidades_dentro / total_acidentes,
    noite_perc = noite / total_acidentes
  )


accidents_grid_redux_localidades <- accidents_grid |>
  st_drop_geometry() |>
  ungroup() |>
  filter(localidades_dentro == 1) |>
  group_by(grid_id) |>
  summarise(
    total_acidentes = n(),
    noite = sum(noite),
    vitimas_mortais30 = sum(vitimas_mortais30, na.rm = TRUE),
    feridos_graves30 = sum(feridos_graves30, na.rm = TRUE),
    feridos_ligeiros30 = sum(feridos_ligeiros30, na.rm = TRUE),
    total_vitimas30 = sum(total_vitimas30, na.rm = TRUE),
    veh_motorizado = sum(veh_motorizado, na.rm = TRUE),
    veh_bicicleta = sum(veh_bicicleta, na.rm = TRUE),
    veh_peoes = sum(veh_peoes, na.rm = TRUE),
    total_veiculos = sum(total_veh, na.rm = TRUE)
  ) |>
  ungroup() |>
  left_join(landuse_grid |> select(id, population), by = c("grid_id" = "id")) |>
  mutate(
    indice_gravidade = ifelse(total_vitimas30 > 0, vitimas_mortais30 / total_vitimas30, NA_real_), # NA se dividido por zero
    indice_gravidade_carro = ifelse(veh_motorizado > 0, vitimas_mortais30 / veh_motorizado, NA_real_), # ignora se não houve carro involvido
    indice_gravidade_bicicleta = ifelse(veh_bicicleta > 0, vitimas_mortais30 / veh_bicicleta, NA_real_),
    indice_gravidade_peoes = ifelse(veh_peoes > 0, vitimas_mortais30 / veh_peoes, NA_real_)
  ) |>
  mutate(
    acidentes_per_1000res = ifelse(population > 2, total_acidentes / population * 1000, NA_real_), # NA se população =< 2
    vitimas_mortais30_per_1000res = ifelse(population > 2, vitimas_mortais30 / population * 1000, NA_real_),
    noite_perc = noite / total_acidentes
  )

### Organizar
accidents_grid_final <- accidents_grid_redux |>
  select(
    grid_id, population, total_acidentes, localidades_dentro, localidades_dentro_perc, noite, noite_perc,
    vitimas_mortais30, feridos_graves30, feridos_ligeiros30, total_vitimas30,
    veh_motorizado, veh_bicicleta, veh_peoes, total_veiculos,
    indice_gravidade, indice_gravidade_carro, indice_gravidade_bicicleta, indice_gravidade_peoes,
    acidentes_per_1000res, vitimas_mortais30_per_1000res
  )

accidents_grid_localidades_final <- accidents_grid_redux_localidades |>
  select(
    grid_id, population, total_acidentes, noite, noite_perc,
    vitimas_mortais30, feridos_graves30, feridos_ligeiros30, total_vitimas30,
    veh_motorizado, veh_bicicleta, veh_peoes, total_veiculos,
    indice_gravidade, indice_gravidade_carro, indice_gravidade_bicicleta, indice_gravidade_peoes,
    acidentes_per_1000res, vitimas_mortais30_per_1000res
  )


# confirm with map
grid |> left_join(accidents_grid_localidades_final, by = c("id" = "grid_id")) |>
  # mapview::mapview(zcol = "total_acidentes", alpha.regions = 0.5)
  mapview::mapview(zcol = "acidentes_per_1000res", alpha.regions = 0.5)

grid |> left_join(accidents_grid_final, by = c("id" = "grid_id")) |>
  # mapview::mapview(zcol = "total_acidentes", alpha.regions = 0.5)
  mapview::mapview(zcol = "vitimas_mortais30", alpha.regions = 0.5)


# ----------------------------
# 4) Exportar csv
# ----------------------------

impt_write(accidents_final, "/safety/accidents_by_freguesia_5years.csv")
impt_write(accidents_localidades_final, "/safety/accidents_by_freguesia_5years_dentrolocalidades.csv")

impt_write(accidents_final_municipio, "/safety/accidents_by_municipio_5years.csv")
impt_write(accidents_final_municipio_localidades, "/safety/accidents_by_municipio_5years_dentrolocalidades.csv")

impt_write(accidents_grid_final, "/safety/accidents_by_grid_5years.csv")
impt_write(accidents_grid_localidades_final, "/safety/accidents_by_grid_5years_dentrolocalidades.csv")
