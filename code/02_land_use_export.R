# export land use auxiliary variables

# load libraries
library(tidyverse)
library(sf)
library(readxl)


# Load Reference Data --------------------------------------------------
# population and census from original census 21
census24_fregmun <- impt_read("census24_fregmun.csv", root = "useful_data")
census24_fregmun <- census24_fregmun |> mutate(freg_id = as.character(freg_id), mun_id = as.character(mun_id))
grid <- impt_read("/geo/grelha_h3_r8.gpkg") |> mutate(id = as.character(id))
grid_freg_mun <- impt_read("grid_nuts.csv", root = "useful_data") |> mutate(grid_id = as.character(grid_id), freg_id = as.character(freg_id), mun_id = as.character(mun_id))
freguesias_geo <- impt_read("freguesias.gpkg", root = "useful_data")
freguesias_area <- freguesias_geo |>
  st_drop_geometry() |>
  mutate(
    area_m2 = area_ha * 10000,
    area_km2 = round(area_ha / 100, 1)
  )

mun_freg_id <- census24_fregmun |>
  select(freg_id, mun_id) |>
  unique()
municipios_area <- freguesias_area |>
  left_join(mun_freg_id, by = c("dtmnfr" = "freg_id")) |>
  group_by(mun_id, municipio) |>
  summarize(
    area_m2 = sum(area_m2),
    area_km2 = sum(area_km2)
  ) |>
  ungroup()

# Helper: sum that returns NA when ALL inputs are NA (unlike sum(na.rm=TRUE) which returns 0)
sum_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# Census, Houses and buildings ----------------------------------------------------
census_pts <- impt_read("/geo/census24_points.gpkg") |>
  select(
    freg_id = dicofre24,
    buildings = N_EDIFICIOS_CLASSICOS,
    buildings_pre1945 = N_EDIFICIOS_CONSTR_ANTES_1945,
    population = N_INDIVIDUOS,
    households = N_NUCLEOS_FAMILIARES, # households
    pop_area_m2 = SHAPE_Area, # census section area for density
    youth = N_INDIVIDUOS_0_14,
    adults1 = N_INDIVIDUOS_15_24,
    adults2 = N_INDIVIDUOS_25_64,
    elderly = N_INDIVIDUOS_65_OU_MAIS,
    women = N_INDIVIDUOS_M
  ) |> 
  mutate(adults = adults1 + adults2) |> 
  select(-adults1, adults2)

volume_pts <- impt_read("/landuse/lisbon_metro_buildings_height.geojson") |>
  select(volume_m3)


# Assign every point to a Grid ID
census_with_grid <- st_join(census_pts, grid["id"], join = st_within)
volume_with_grid <- st_join(volume_pts, grid["id"], join = st_within)

# Create Master Tabular Summaries
# Aggregate by grid_id first
grid_stats <- census_with_grid |>
  st_drop_geometry() |>
  group_by(id) |>
  summarise(
    buildings_pre1945 = sum(buildings_pre1945, na.rm = TRUE),
    buildings = sum(buildings, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    population_density = sum(pop_area_m2, na.rm = TRUE),
    households = sum(households, na.rm = TRUE),
    youth = sum(youth, na.rm = TRUE),
    adults = sum(adults, na.rm = TRUE),
    elderly = sum(elderly, na.rm = TRUE),
    women = sum(women, na.rm = TRUE)
  ) |>
  full_join(
    volume_with_grid |>
      st_drop_geometry() |>
      group_by(id) |>
      summarise(buildings_volume_m3 = sum(volume_m3, na.rm = TRUE)),
    by = "id"
  ) |>
  rename(grid_id = id) |>
  ungroup()

# Join the cross-reference (freg_id, mun_id) to these stats
# Grid cells with no census centroid inside them keep NA (no false zeros)
master_stats <- grid_freg_mun |>
  left_join(grid_stats |> mutate(grid_id = as.character(grid_id)), by = "grid_id") |>
  mutate(
    grid_id = as.character(grid_id),
    freg_id = as.character(freg_id),
    mun_id = as.character(mun_id)
  )

# Export Levels

# ## GRID LEVEL -- SEE 02_census_grid_with_cos.R
# landuse_grid <- grid |>
#   mutate(id = as.character(id)) |>
#   left_join(master_stats, by = c("id" = "grid_id")) |>
#   mutate(
#     area_m2 = as.numeric(st_area(geom)),
#     volume_density = round(buildings_volume_m3 / area_m2, 2),
#     population_density = round(population / (area_m2 / 1e6), 1), # pop/km²
#     youth_ratio = round(youth / population * 100, 2),
#     elderly_ratio = round(elderly / population * 100, 2),
#     women_percentage = round(women / population * 100, 2)
#   ) |>
#   mutate(
#     area_km2 = round(area_m2 / 1e6, 2),
#     buildings_pre1945_percentage = round(buildings_pre1945 / buildings * 100, 2)
#   )
# # mapview(landuse_grid, zcol="volume_density")
# landuse_grid <- landuse_grid |>
#   st_drop_geometry() |>
#   select(
#     id, area_km2,
#     buildings, buildings_pre1945, buildings_pre1945_percentage,
#     buildings_volume_m3, volume_density,
#     population, population_density, households,
#     youth_ratio, elderly_ratio, women_percentage
#   )
# impt_write(landuse_grid, "/landuse/landuse_grid.csv")


## FREGUESIA LEVEL
landuse_freguesias <- census24_fregmun |>
  left_join(
    master_stats |>
      select(freg_id, buildings_volume_m3) |>
      group_by(freg_id) |>
      summarise(
        buildings_volume_m3 = sum_na(buildings_volume_m3)
      )
  ) |>
  left_join(freguesias_area |> select(dtmnfr, area_m2, area_km2), by = c("freg_id" = "dtmnfr")) |>
  mutate(
    buildings_volume_m3 = round(buildings_volume_m3),
    volume_density = round(buildings_volume_m3 / area_m2, 2),
    buildings_pre1945_percentage = round(buildings_pre1945 / buildings * 100, 2)
  ) |>
  mutate(
    population_density = round(population / area_km2, 1), # pop/km²
    youth_ratio        = round(youth / population * 100, 2),
    elderly_ratio      = round(elderly / population * 100, 2),
    youth_dependency   = round(youth / adults * 100, 2),
    elderly_dependency = round(elderly / adults * 100, 2),
    women_percentage   = round(women / population * 100, 2)
  )

landuse_freguesias <- landuse_freguesias |> select(
  freg_id, area_km2,
  buildings, buildings_pre1945, buildings_pre1945_percentage,
  buildings_volume_m3, volume_density,
  population, population_density, households,
  youth_ratio, elderly_ratio, youth_dependency, elderly_dependency,
  women_percentage
)
impt_write(landuse_freguesias, "/landuse/landuse_freguesias.csv")

## MUNICIPALITY LEVEL
landuse_municipios <- census24_fregmun |>
  left_join(
    master_stats |>
      select(freg_id, buildings_volume_m3) |>
      group_by(freg_id) |>
      summarise(
        buildings_volume_m3 = sum_na(buildings_volume_m3)
      ) |>
      ungroup()
  ) |>
  group_by(mun_id) |>
  summarise(
    population = sum(population, na.rm = TRUE),
    households = sum(households, na.rm = TRUE),
    youth = sum(youth, na.rm = TRUE),
    adults = sum(adults, na.rm = TRUE),
    elderly = sum(elderly, na.rm = TRUE),
    women = sum(women, na.rm = TRUE),
    buildings = sum(buildings, na.rm = TRUE),
    buildings_pre1945 = sum(buildings_pre1945, na.rm = TRUE),
    buildings_volume_m3 = sum(buildings_volume_m3, na.rm = TRUE)
  ) |>
  left_join(municipios_area |> select(mun_id, area_m2, area_km2), by = "mun_id") |>
  mutate(
    buildings_volume_m3 = round(buildings_volume_m3),
    volume_density = round(buildings_volume_m3 / area_m2, 2),
    buildings_pre1945_percentage = round(buildings_pre1945 / buildings * 100, 2)
  ) |>
  mutate(
    population_density = round(population / area_km2, 1), # pop/km²
    youth_ratio        = round(youth / population * 100, 2),
    elderly_ratio      = round(elderly / population * 100, 2),
    youth_dependency   = round(youth / adults * 100, 2),
    elderly_dependency = round(elderly / adults * 100, 2),
    women_percentage   = round(women / population * 100, 2)
  )

landuse_municipios <- landuse_municipios |> select(
  mun_id, area_km2,
  buildings, buildings_pre1945, buildings_pre1945_percentage,
  buildings_volume_m3, volume_density,
  population, population_density, households,
  youth_ratio, elderly_ratio, youth_dependency, elderly_dependency,
  women_percentage
)
impt_write(landuse_municipios, "/landuse/landuse_municipios.csv")


# Final check
sum(census_pts$population) # 2870208
sum(landuse_grid$population, na.rm = TRUE) # 2850646 # some census bgri centroids fall outside the grid cells, so we lose some 19.562 population
sum(landuse_municipios$population) # 2870206 # 2 missing due round when separating new freguesias
sum(landuse_freguesias$population) # 2870206

sum(landuse_grid$households, na.rm = TRUE) # 834277
sum(landuse_freguesias$households) # 839934
sum(landuse_municipios$households) # 839934
summary(landuse_freguesias$population / landuse_freguesias$households) # 3.43?? people per household
summary(landuse_municipios$population / landuse_municipios$households) # 3.37?? people per household
sum(census_pts$population) / sum(census_pts$households) # 3.147 people per household in the original census data, so we have some discrepancies here that may be due to the way we assign census points to freguesias and municipalities, and how we handle missing data. This is something to keep in mind when interpreting the results, but overall the numbers are in a reasonable range.

# Income, Gini, housing costs -----------------------------------

## income
income_freguesias <- impt_read("BaseDados_INE/Rendimento_agregado_2023.xls",
  sheet = "freg", col_types = c("skip", "numeric", "text")
)
income_freguesias <- income_freguesias |> filter(freg_id %in% grid_freg_mun$freg_id) # filter only freguesias that are in the grid, to avoid join problems later

income_municipios <- impt_read("BaseDados_INE/Rendimento_agregado_2023.xls",
  sheet = "mun", col_types = c("skip", "numeric", "text", "skip", "skip")
)

income_grid <- income_freguesias |>
  left_join(grid_freg_mun |> select(freg_id, grid_id)) |>
  select(-freg_id)


## housing costs
housing_freguesias <- impt_read("BaseDados_INE/Habitacao_custos_2021.xls",
  sheet = "freg", col_types = c("skip", "text", "numeric")
)
housing_freguesias <- housing_freguesias |> filter(freg_id %in% grid_freg_mun$freg_id) # filter only freguesias that are in the grid, to avoid join problems later

housing_municipios <- impt_read("BaseDados_INE/Habitacao_custos_2021.xls",
  sheet = "mun", col_types = c("skip", "text", "numeric")
) |>
  mutate(housing_costs = round(housing_costs, 2))

housing_grid <- housing_freguesias |>
  left_join(grid_freg_mun |> select(freg_id, grid_id)) |>
  select(-freg_id)

## Gini coefficient
gini_municipios <- impt_read("BaseDados_INE/Rendimento_agregado_2023.xls", # source ERendimentoNLocal2023.xlsx INE 2023
  sheet = "mun", col_types = c("skip", "skip", "text", "numeric", "numeric")
) |>
  mutate(
    gini_coef = round(gini_coef, 1),
    palma_ratio = round(palma_ratio, 2)
  )

gini_freguesias <- grid_freg_mun |>
  select(freg_id, mun_id) |>
  distinct() |>
  na.omit() |>
  left_join(gini_municipios) |>
  select(freg_id, gini_coef, palma_ratio)

gini_grid <- grid_freg_mun |>
  select(grid_id, mun_id) |>
  # distinct() |> na.omit() |>
  left_join(gini_municipios) |>
  select(grid_id, gini_coef, palma_ratio)


## Export
income_grid |>
  left_join(housing_grid) |>
  left_join(gini_grid) |>
  select(grid_id, income_hh, gini_coef, palma_ratio, housing_costs) |>
  impt_write("/landuse/grid_income_housing_gini.csv")
income_freguesias |>
  left_join(housing_freguesias) |>
  left_join(gini_freguesias) |>
  select(freg_id, income_hh, gini_coef, palma_ratio, housing_costs) |>
  impt_write("/landuse/freguesias_income_housing_gini.csv")
income_municipios |>
  left_join(housing_municipios) |>
  left_join(gini_municipios) |>
  select(mun_id, income_hh, gini_coef, palma_ratio, housing_costs) |>
  impt_write("/landuse/municipios_income_housing_gini.csv")
