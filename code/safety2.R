library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# ----------------------------
# 0) IMPT URL helper
# ----------------------------
DATA_LOCATION <- "/data/IMPT" 
# DATA_LOCATION <- "https://impt.server.ushift.pt" # se estiveres local
API_KEY <- Sys.getenv("IMPT_DATA_KEY")

IMPT_URL <- function(path) {
  if (startsWith(DATA_LOCATION, "http")) {
    if (API_KEY == "") stop("IMPT_DATA_KEY env var não está definida. Define e reinicia o R.")
    return(sprintf("%s%s?key=%s", DATA_LOCATION, path, API_KEY))
  }
  sprintf("%s%s", DATA_LOCATION, path)
}

# ----------------------------
# 1) Ler freguesias (2024) e acidentes (IG)
# ----------------------------
freguesias <- st_read(IMPT_URL("/geo/freguesias_2024_unique.gpkg"), quiet = TRUE)

acidentes_path <- IMPT_URL("/safety/13-SegurançaRodoviária.gpkg")
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

ig_freg = ig_freg |> filter(!is.na(freguesia)) # remove os 173 sem freguesia (pontes!) (que são poucos, e não vale a pena resolver por nearest)

# only last 5 years
table(ig_freg$Ano)
ig_freg = ig_freg |> filter(Ano %in% c(2019:2023))

# select only the variables that matter
names(ig_freg)
ig_redux = ig_freg |> select(dtmnfr, freguesia, Ano, VM.30d, X_VM.30d, FG.30d, X_FG.30d, FL.30d, X_FL.30d, X_Veículo,
                            X_Veícul_1, X_Ciclomoto, X_Desconhec, X_Máquina,  X_Motociclo, X_Motocic_1, X_Quadricic, X_Triciclo,
                            X_Veícul_2, X_Veícul_3, X_Veícul_4, X_Velocípe, X_Velocí_1, X_nd, X_Total.Ger, Total.Peõ,
                            X_IG, Localizaç, Luminosida)
accidents = ig_redux |>
  mutate(vitimas_mortais30 = as.integer(X_VM.30d),
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



accidents_redux = accidents |>
  st_drop_geometry() |> 
  ungroup() |> 
  group_by(dtmnfr, freguesia) |>
  summarise(total_acidentes = n(), # total 5 anos
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
accidents_redux = accidents_redux |> 
  left_join(freguesias |> st_drop_geometry() |> select(dtmnfr, municipio), by = "dtmnfr")


### Join with population
accidents_redux = accidents_redux |> 
  left_join(freguesias_aggregated |> st_drop_geometry() |> select(id, population = census_residents) |> 
              mutate(dtmnfr = as.character(id))) |> 
  select(-id)




# ==========================================================
# (5) ÍNDICE DE GRAVIDADE
# VM30 / (acidentes com vítimas: mortos + feridos)
# ==========================================================

accidents_redux = accidents_redux |> 
  mutate(
    indice_gravidade = vitimas_mortais30 / total_vitimas30,
    indice_gravidade_carro = ifelse(veh_motorizado > 0, vitimas_mortais30 / veh_motorizado, NA_real_), # ignora se não houve carro involvido
    indice_gravidade_bicicleta = ifelse(veh_bicicleta > 0, vitimas_mortais30 / veh_bicicleta, NA_real_),
    indice_gravidade_peoes = ifelse(veh_peoes > 0, vitimas_mortais30 / veh_peoes, NA_real_))

# NA means that there were no vehicles of that type involved, so we can't calculate a severity index for that mode.

accidents_redux = accidents_redux |> 
  mutate(
    acidentes_per_1000res = total_acidentes / population * 1000,
    vitimas_mortais30_per_1000res = vitimas_mortais30 / population *1000,
    localidades_dentro_perc = localidades_dentro / total_acidentes,
    noite_perc = noite / total_acidentes
  )






# apenas dentro das localidades -------------------------------------------

accidents_redux_localidades = accidents |>
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
  left_join(freguesias |> st_drop_geometry() |> select(dtmnfr, municipio),
            by = "dtmnfr") |>
  left_join(
    freguesias_aggregated |> st_drop_geometry() |> select(id, population = census_residents) |>
      mutate(dtmnfr = as.character(id))
  ) |>
  select(-id) |>
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
accidents_final = accidents_redux |> 
  select(dtmnfr, freguesia, municipio, population, total_acidentes, localidades_dentro, localidades_dentro_perc, noite, noite_perc,
         vitimas_mortais30, feridos_graves30, feridos_ligeiros30, total_vitimas30,
         veh_motorizado, veh_bicicleta, veh_peoes, total_veiculos,
         indice_gravidade, indice_gravidade_carro, indice_gravidade_bicicleta, indice_gravidade_peoes,
         acidentes_per_1000res, vitimas_mortais30_per_1000res) |> 
  arrange(municipio, freguesia)

accidents_localidades_final = accidents_redux_localidades |> 
  select(dtmnfr, freguesia, municipio, population, total_acidentes, noite, noite_perc,
         vitimas_mortais30, feridos_graves30, feridos_ligeiros30, total_vitimas30,
         veh_motorizado, veh_bicicleta, veh_peoes, total_veiculos,
         indice_gravidade, indice_gravidade_carro, indice_gravidade_bicicleta, indice_gravidade_peoes,
         acidentes_per_1000res, vitimas_mortais30_per_1000res) |> 
  arrange(municipio, freguesia)


# ----------------------------
# 4) Exportar csv
# ----------------------------

write.csv(accidents_final, "data/accidents_by_freguesia_5years.csv", row.names = FALSE)
write.csv(accidents_final, "data/accidents_by_freguesia_5years_dentrolocalidades.csv", row.names = FALSE)


