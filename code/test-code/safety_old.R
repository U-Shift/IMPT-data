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
mapview::mapview(ig_freg) + mapview::mapview(freguesias, alpha.regions = 0.2)

ig_freg = ig_freg |> filter(!is.na(freguesia)) # remove os 173 sem freguesia (pontes!) (que são poucos, e não vale a pena resolver por nearest)

# ----------------------------
# 3) Variáveis auxiliares (modos, VM30, filtros arruamento/noite)
# (sem "limpeza" de nomes; só conversão numérica)
# ----------------------------
ig_tab <- ig_freg %>%
  st_drop_geometry() %>%
  mutate(
    carro = X_Veículo > 0,
    bicicleta = X_Velocípe > 0,
    pedestre = grepl("Atropelamento", Natureza),
    VM30 = as.numeric(VM.30d),
    
    arruamento = grepl("Arruamento", Tipos.Vias, ignore.case = TRUE),
    noturno = grepl("Noite", Luminosida, ignore.case = TRUE)
  )

# ==========================================================
# (1) Nº de acidentes por ANO e por MODO (wide)
# ==========================================================
acidentes_ano_modo_wide <- ig_tab %>%
  group_by(dtmnfr, freguesia, Ano) %>%
  summarise(
    acid_carro = sum(carro, na.rm = TRUE),
    acid_bicicleta = sum(bicicleta, na.rm = TRUE),
    acid_pedestre = sum(pedestre, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(dtmnfr, freguesia),
    names_from = Ano,
    values_from = c(acid_carro, acid_bicicleta, acid_pedestre),
    names_glue = "{.value}_{Ano}",
    values_fill = 0
  ) %>%
  arrange(freguesia)

# ==========================================================
# (2) Nº de VÍTIMAS MORTAIS (30 dias) por MODO (por freguesia)
# ==========================================================
vitimas_mortais_modo <- ig_tab %>%
  group_by(dtmnfr, freguesia) %>%
  summarise(
    VM30_carro = sum(VM30[carro], na.rm = TRUE),
    VM30_bicicleta = sum(VM30[bicicleta], na.rm = TRUE),
    VM30_pedestre = sum(VM30[pedestre], na.rm = TRUE),
    VM30_total = sum(VM30, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(VM30_total))

# ==========================================================
# (3) Nº de ACIDENTES em ARRUAMENTOS por MODO (por freguesia)
# ==========================================================
acidentes_arruamentos_modo <- ig_tab %>%
  filter(arruamento) %>%
  group_by(dtmnfr, freguesia) %>%
  summarise(
    acid_arr_carro = sum(carro, na.rm = TRUE),
    acid_arr_bicicleta = sum(bicicleta, na.rm = TRUE),
    acid_arr_pedestre = sum(pedestre, na.rm = TRUE),
    total_arruamento = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_arruamento))

# ==========================================================
# (4) Nº de ACIDENTES NOTURNOS por MODO (por freguesia)
# ==========================================================
acidentes_noturnos_modo <- ig_tab %>%
  filter(noturno) %>%
  group_by(dtmnfr, freguesia) %>%
  summarise(
    acid_noite_carro = sum(carro, na.rm = TRUE),
    acid_noite_bicicleta = sum(bicicleta, na.rm = TRUE),
    acid_noite_pedestre = sum(pedestre, na.rm = TRUE),
    total_noturno = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_noturno))

# ----------------------------
# 4) Exportar Excel (4 folhas)
# ----------------------------
wb <- createWorkbook()
addWorksheet(wb, "Acidentes_ano_modo_WIDE")
addWorksheet(wb, "Vitimas_mortais_modo")
addWorksheet(wb, "Arruamentos_modo")
addWorksheet(wb, "Noturnos_modo")

writeData(wb, "Acidentes_ano_modo_WIDE", acidentes_ano_modo_wide)
writeData(wb, "Vitimas_mortais_modo", vitimas_mortais_modo)
writeData(wb, "Arruamentos_modo", acidentes_arruamentos_modo)
writeData(wb, "Noturnos_modo", acidentes_noturnos_modo)

saveWorkbook(wb, "indicadores_acidentes_por_freguesia_nova.xlsx", overwrite = TRUE)

# ==========================================================
# (5) ÍNDICE DE GRAVIDADE
# VM30 / (acidentes com vítimas: mortos + feridos)
# ==========================================================

indice_gravidade_freg <- ig_freg %>%
  st_drop_geometry() %>%
  mutate(
    VM30 = as.numeric(VM.30d),
    FG30 = as.numeric(FG.30d),
    FL30 = as.numeric(FL.30d),
    acidente_com_vitimas = (VM30 + FG30 + FL30) > 0
  ) %>%
  group_by(dtmnfr, freguesia) %>%
  summarise(
    VM30_total = sum(VM30, na.rm = TRUE),
    acidentes_com_vitimas = sum(acidente_com_vitimas, na.rm = TRUE),
    indice_gravidade = VM30_total / acidentes_com_vitimas,
    .groups = "drop"
  ) %>%
  arrange(desc(indice_gravidade))

indice_gravidade_freg
write.xlsx(
  indice_gravidade_freg,
  file = "indice_gravidade_por_freguesia.xlsx",
  rowNames = FALSE
)

# ==========================================================
# (X) VM30 / (todas as vítimas) por MODO e por FREGUESIA
# ==========================================================
vm30_sobre_todas_vitimas_modo_freg <- ig_freg %>%
  st_drop_geometry() %>%
  mutate(
    # modos (mantém a tua definição)
    carro     = X_Veículo  > 0,
    bicicleta = X_Velocípe > 0,
    pedestre  = grepl("Atropelamento", Natureza),
    
    # vítimas (30 dias)
    VM30 = as.numeric(VM.30d),
    FG30 = as.numeric(FG.30d),
    FL30 = as.numeric(FL.30d),
    
    # total de vítimas no acidente
    total_vitimas_30 = VM30 + FG30 + FL30
  ) %>%
  group_by(dtmnfr, freguesia) %>%
  summarise(
    # --- CARRO ---
    VM30_carro = sum(VM30[carro], na.rm = TRUE),
    vitimas_carro = sum(total_vitimas_30[carro], na.rm = TRUE),
    indice_carro = ifelse(vitimas_carro > 0, VM30_carro / vitimas_carro, NA_real_),
    
    # --- BICICLETA ---
    VM30_bicicleta = sum(VM30[bicicleta], na.rm = TRUE),
    vitimas_bicicleta = sum(total_vitimas_30[bicicleta], na.rm = TRUE),
    indice_bicicleta = ifelse(vitimas_bicicleta > 0, VM30_bicicleta / vitimas_bicicleta, NA_real_),
    
    # --- PEDESTRE ---
    VM30_pedestre = sum(VM30[pedestre], na.rm = TRUE),
    vitimas_pedestre = sum(total_vitimas_30[pedestre], na.rm = TRUE),
    indice_pedestre = ifelse(vitimas_pedestre > 0, VM30_pedestre / vitimas_pedestre, NA_real_),
    
    .groups = "drop"
  ) %>%
  arrange(freguesia)

vm30_sobre_todas_vitimas_modo_freg
write.xlsx(
  vm30_sobre_todas_vitimas_modo_freg,
  file = "indice_VM30_sobre_todas_vitimas_por_modo_freguesia.xlsx",
  rowNames = FALSE
)


#iteração-----------------------------------------------------------------------
# ==========================================================
# Índice de gravidade: VM30 / (VM30 + FG30 + FL30)
# por MODO e por FREGUESIA
# ==========================================================
indice_gravidade_classico_modo_freg <- ig_freg %>%
  st_drop_geometry() %>%
  mutate(
    # modos (mantém a tua definição)
    carro     = X_Veículo  > 0,
    bicicleta = X_Velocípe > 0,
    pedestre  = grepl("Atropelamento", Natureza),
    
    # vítimas a 30 dias
    VM30 = as.numeric(VM.30d),
    FG30 = as.numeric(FG.30d),
    FL30 = as.numeric(FL.30d),
    
    total_vitimas_30 = VM30 + FG30 + FL30
  ) %>%
  group_by(dtmnfr, freguesia) %>%
  summarise(
    # --- CARRO ---
    VM30_carro = sum(VM30[carro], na.rm = TRUE),
    vitimas_carro = sum(total_vitimas_30[carro], na.rm = TRUE),
    IG_carro = ifelse(vitimas_carro > 0, VM30_carro / vitimas_carro, NA_real_),
    
    # --- BICICLETA ---
    VM30_bicicleta = sum(VM30[bicicleta], na.rm = TRUE),
    vitimas_bicicleta = sum(total_vitimas_30[bicicleta], na.rm = TRUE),
    IG_bicicleta = ifelse(vitimas_bicicleta > 0, VM30_bicicleta / vitimas_bicicleta, NA_real_),
    
    # --- PEDESTRE ---
    VM30_pedestre = sum(VM30[pedestre], na.rm = TRUE),
    vitimas_pedestre = sum(total_vitimas_30[pedestre], na.rm = TRUE),
    IG_pedestre = ifelse(vitimas_pedestre > 0, VM30_pedestre / vitimas_pedestre, NA_real_),
    
    # --- TOTAL (todos os modos juntos) ---
    VM30_total = sum(VM30, na.rm = TRUE),
    vitimas_total = sum(total_vitimas_30, na.rm = TRUE),
    IG_total = ifelse(vitimas_total > 0, VM30_total / vitimas_total, NA_real_),
    
    .groups = "drop"
  ) %>%
  arrange(freguesia)

indice_gravidade_classico_modo_freg
