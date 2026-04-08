
### Script inquerito workshop TML

library(dplyr)
library(tidyr)
library(readr)
library(mapview)
library(ggplot2)
library(tidyverse)
library(ggrepel)

# 1. Carregar os dados (StringsAsFactors garante que tratamos texto como texto)
respostas_raw <- read_csv("WORKSHOP/IMPTWorkshop (Responses).csv")

# 2. Identificar as colunas que contêm os nomes das freguesias
# Procuramos por qualquer coluna que contenha "Freguesia" no nome
colunas_freguesia <- grep("Freguesia", names(respostas_raw), value = TRUE)

# 3. Unificar com Coalesce (Versão Robusta)
respostas_limpas <- respostas_raw %>%
  mutate(
    # Usamos o across() dentro do coalesce de forma direta
    freguesia_unificada = do.call(coalesce, select(., all_of(colunas_freguesia)))
  ) %>%
  # Limpeza: Remove espaços, converte para maiúsculas e remove NA's acidentais
  mutate(
    freguesia_unificada = (trimws(as.character(freguesia_unificada)))
  ) 

IMPTWorkshop <- respostas_limpas %>%
  select(-all_of(colunas_freguesia)) %>% # Remove as colunas originais de freguesia
  rename(Freguesia = freguesia_unificada) # Renomeia a nova coluna unificada

### Rename the columns so that they are more intuitive e.g. Accessibility_index, Affordability_index, etc.
IMPTWorkshop <- IMPTWorkshop %>%
  rename(`Accessibility_Index` = `Como avalia a acessibilidade na sua freguesia?`,
         `Municipio` = `Selecione o seu Município (Concelho):`, 
         `Affordability_Index` = `Como avalia os gastos em transporte na sua freguesia?`,
         `Safety_Index` = `Como avalia a segurança rodoviária na sua freguesia?`,
         `Mobility_Index` = `Como avalia a mobilidade na sua freguesia?`,
         `IMPT` = `Como avalia o IMPT global na sua freguesia?`,
         `freguesia` = `Freguesia`
         ) |> 
  #Multiply numeric columns by 10
  mutate(Accessibility_Index = as.numeric(Accessibility_Index) * 10,
         Affordability_Index = as.numeric(Affordability_Index) * 10,
         Safety_Index = as.numeric(Safety_Index) * 10,
         Mobility_Index = as.numeric(Mobility_Index) * 10,
         IMPT = 100 - as.numeric(IMPT) * 10)  |> 
  group_by(freguesia, Municipio) |> 
  summarise(IMPT = mean(IMPT, na.rm = TRUE),
            Accessibility_Index = mean(Accessibility_Index, na.rm = TRUE),
            Affordability_Index = mean(Affordability_Index, na.rm = TRUE),
            Safety_Index = mean(Safety_Index, na.rm = TRUE),
            Mobility_Index = mean(Mobility_Index, na.rm = TRUE),
            .groups = "drop")


# Read freguesias geopackge

Freguesias <- st_read("WORKSHOP/freguesias_2024_unique.gpkg") |> 
  select("dtmnfr", "freguesia", "municipio", "geom")


### Visualizar en el mapa

# Unir os dados do workshop com as freguesias usando o nome da freguesia
dados_mapa <- Freguesias %>%
  left_join(IMPTWorkshop, by = "freguesia") |> 
  select( -Municipio) 
  


mapview(dados_mapa, zcol = "IMPT", na.color = "transparent") + 
  mapview(dados_mapa, zcol = "Accessibility_Index", hide = TRUE , na.color = "transparent") +
  mapview(dados_mapa, zcol = "Affordability_Index", hide = TRUE , na.color = "transparent") +
  mapview(dados_mapa, zcol = "Safety_Index", hide = TRUE , na.color = "transparent") +
  mapview(dados_mapa, zcol = "Mobility_Index", hide = TRUE , na.color = "transparent")



###### IMPORT FREGUESIAS GEOJSON

Resultados_IMPT <- st_read("WORKSHOP/freguesias_aggregated.geojson") |> 
  select("id","name", "IMPT_entropy_pca_pass", "Accessibility_Index", "Affordability_Index_pass", "Safety_Index", "Mobility_Index") |> 
  rename(freguesia = name, 
         IMPT = IMPT_entropy_pca_pass, 
         Affordability_Index = Affordability_Index_pass,
         dtmnfr = id) 


###### COMPARE IMPT WORKSHOP vs RESULTADOS_IMPT USING SCATTER PLOT POR DIMENSÃO 

# Step 1: Add the dtmnfr to the Survey using the Reference Table
# (Replace 'freguesia_name_col' with the column name in your survey)
IMPTWorkshop_dtmnfr <- IMPTWorkshop %>%
  left_join(Freguesias %>% select(dtmnfr, freguesia), 
            by = c("freguesia" = "freguesia"))

# Step 2: Now perform the Inner Join for the comparison
# This keeps only the freguesias that were in the survey
comparison_final <- IMPTWorkshop_dtmnfr %>%
  inner_join(Resultados_IMPT, by = "dtmnfr", suffix = c("_survey", "_model")) |> 
  rename(freguesia = freguesia_survey)


########## IMPT COMPARISON ###############

# 1. Calculate the Gaps
comparison_final <- comparison_final %>%
  mutate(
    # We use IMPT CONTRASTE
    gap = IMPT_model - IMPT_survey,
    abs_gap = abs(gap),
    # Label only the top 10 biggest discrepancies to keep the graph clean
    label_flag = if_else(rank(-abs_gap) <= 10, freguesia, "")
  ) |> 
  mutate(
    gap_acc = Accessibility_Index_model - Accessibility_Index_survey,
    abs_gap_acc = abs(gap_acc),
    gap_aff = Affordability_Index_model - Affordability_Index_survey,
    abs_gap_aff = abs(gap_aff),
    gap_saf = Safety_Index_model - Safety_Index_survey,
    abs_gap_saf = abs(gap_saf),
    gap_mob = Mobility_Index_model - Mobility_Index_survey,
    abs_gap_mob = abs(gap_mob),
    label_flag_acc = if_else(rank(-abs_gap_acc) <= 10, freguesia, ""),
    label_flag_aff = if_else(rank(-abs_gap_aff) <= 10, freguesia, ""),
    label_flag_saf = if_else(rank(-abs_gap_saf) <= 10, freguesia, ""),
    label_flag_mob = if_else(rank(-abs_gap_mob) <= 10, freguesia, "")
  )
  

# 2. Create the Discrepancy Table
top_gaps <- comparison_final %>%
  select(freguesia, IMPT_model, IMPT_survey, gap) %>%
  arrange(desc(abs(gap))) %>%
  head(10)

print(top_gaps)

top_gaps_acc <- comparison_final %>%
  select(freguesia, Accessibility_Index_model, Accessibility_Index_survey, gap_acc) %>%
  arrange(desc(abs(gap_acc))) %>%
  head(10)

print(top_gaps_acc)

top_gaps_aff <- comparison_final %>%
  select(freguesia, Affordability_Index_model, Affordability_Index_survey, gap_aff) %>%
  arrange(desc(abs(gap_aff))) %>%
  head(10)

print(top_gaps_aff)

top_gaps_saf <- comparison_final %>%
  select(freguesia, Safety_Index_model, Safety_Index_survey, gap_saf) |> 
  arrange(desc(abs(gap_saf))) %>%
  head(10)

print(top_gaps_saf)
  
top_gaps_mob <- comparison_final %>%
  select(freguesia, Mobility_Index_model, Mobility_Index_survey, gap_mob) %>%
  arrange(desc(abs(gap_mob))) %>%
  head(10)  

print(top_gaps_mob)

####### Graph results

ggplot(comparison_final, aes(x = IMPT_model, y = IMPT_survey)) +
  # 1. The "Perfect Agreement" Line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60", size = 1) +
  
  # 2. The Data Points
  geom_point(aes(size = abs_gap, color = gap), alpha = 0.7) +
  
  # 3. Coloring: Orange for 'Model > Survey', Blue for 'Survey > Model'
  scale_color_gradient2(low = "#FF8A65", mid = "#E0E0E0", high = "#1E88E5", midpoint = 0) +
  
  # 4. Smart Labels (prevents text from overlapping points)
  geom_text_repel(aes(label = label_flag), size = 3.5, fontface = "bold") +
  
  # 5. Formatting
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  labs(
    title = "IMPT: Validação de Resultados (Modelo vs. Percepção)",
    subtitle = "Linha tracejada indica concordância perfeita. Pontos rotulados indicam discrepâncias críticas.",
    x = "IMPT Calculado (Dados Estatísticos)",
    y = "IMPT Percebido (Workshop Municípios)",
    color = "Sentido do Desvio",
    size = "Magnitude do Erro"
  ) +
  theme(legend.position = "bottom")



################## COMPARE DIMENSIONS ##################


#### Accessibility
ggplot(comparison_final, aes(x = Accessibility_Index_model, y = Accessibility_Index_survey)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60", size = 1) +
  geom_point(aes(size = abs_gap_acc, color = gap_acc), alpha = 0.7) +
  scale_color_gradient2(low = "#FF8A65", mid = "#E0E0E0", high = "#1E88E5", midpoint = 0) +
  geom_text_repel(aes(label = label_flag_acc), size = 3.5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  labs(
    title = "Accessibility Index: Validação de Resultados (Modelo vs. Percepção)",
    subtitle = "Linha tracejada indica concordância perfeita. Pontos rotulados indicam discrepâncias críticas.",
    x = "Accessibility Index Calculado (Dados Estatísticos)",
    y = "Accessibility Index Percebido (Workshop Municípios)",
    color = "Sentido do Desvio",
    size = "Magnitude do Erro"
  ) +
  theme(legend.position = "bottom")


### Mobility

ggplot(comparison_final, aes(x = Mobility_Index_model, y = Mobility_Index_survey)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60", size = 1) +
  geom_point(aes(size = abs_gap_mob, color = gap_mob), alpha = 0.7) +
  scale_color_gradient2(low = "#FF8A65", mid = "#E0E0E0", high = "#1E88E5", midpoint = 0) +
  geom_text_repel(aes(label = label_flag_mob), size = 3.5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  labs(
    title = "Mobility Index: Validação de Resultados (Modelo vs. Percepção)",
    subtitle = "Linha tracejada indica concordância perfeita. Pontos rotulados indicam discrepâncias críticas.",
    x = "Mobility Index Calculado (Dados Estatísticos)",
    y = "Mobility Index Percebido (Workshop Municípios)",
    color = "Sentido do Desvio",
    size = "Magnitude do Erro"
  ) +
  theme(legend.position = "bottom")


#### Affordability

ggplot(comparison_final, aes(x = Affordability_Index_model, y = Affordability_Index_survey)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60", size = 1) +
  geom_point(aes(size = abs_gap_aff, color = gap_aff), alpha = 0.7) +
  scale_color_gradient2(low = "#FF8A65", mid = "#E0E0E0", high = "#1E88E5", midpoint = 0) +
  geom_text_repel(aes(label = label_flag_aff), size = 3.5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  labs(
    title = "Affordability Index: Validação de Resultados (Modelo vs. Percepção)",
    subtitle = "Linha tracejada indica concordância perfeita. Pontos rotulados indicam discrepâncias críticas.",
    x = "Affordability Index Calculado (Dados Estatísticos)",
    y = "Affordability Index Percebido (Workshop Municípios)",
    color = "Sentido do Desvio",
    size = "Magnitude do Erro"
  ) +
  theme(legend.position = "bottom")



##### Safety

ggplot(comparison_final, aes(x = Safety_Index_model, y = Safety_Index_survey)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60", size = 1) +
  geom_point(aes(size = abs_gap_saf, color = gap_saf), alpha = 0.7) +
  scale_color_gradient2(low = "#FF8A65", mid = "#E0E0E0", high = "#1E88E5", midpoint = 0) +
  geom_text_repel(aes(label = label_flag_saf), size = 3.5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  labs(
    title = "Safety Index: Validação de Resultados (Modelo vs. Percepção)",
    subtitle = "Linha tracejada indica concordância perfeita. Pontos rotulados indicam discrepâncias críticas.",
    x = "Safety Index Calculado (Dados Estatísticos)",
    y = "Safety Index Percebido (Workshop Municípios)",
    color = "Sentido do Desvio",
    size = "Magnitude do Erro"
  ) +
  theme(legend.position = "bottom")



