# export land use auxiliary variables

# load libraries
library(tidyverse)
library(sf)


# Load Reference Data --------------------------------------------------
grid <- st_read("/data/IMPT/geo/grelha_h3_r8.gpkg") |> mutate(id = as.character(id))
grid_freg_mun <- read.csv("useful_data/grid_nuts.csv") |> mutate(grid_id = as.character(grid_id), freg_id = as.character(freg_id), mun_id = as.character(mun_id))
freguesias_geo <- st_read("useful_data/freguesias.gpkg")
municipios_geo <- st_read("/data/IMPT/geo/municipios_2024.gpkg")
municipios_id <- read.csv("useful_data/municipios_id.csv")
municipios_geo <- municipios_geo |>
  left_join(municipios_id) |>
  mutate(mun_id = as.character(mun_id))

# Helper: sum that returns NA when ALL inputs are NA (unlike sum(na.rm=TRUE) which returns 0)
sum_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# Census, Houses and buildings ----------------------------------------------------
census_pts = st_read("/data/IMPT/geo/census24_points.gpkg") |> 
  select(
    buildings_pre1945   = N_EDIFICIOS_CONSTR_ANTES_1945,
    population          = N_INDIVIDUOS,
    pop_area_m2         = SHAPE_Area,       # census section area for density
    youth               = N_INDIVIDUOS_0_14,
    elderly             = N_INDIVIDUOS_65_OU_MAIS,
    women               = N_INDIVIDUOS_M
  )
volume_pts <- st_read("/data/IMPT/landuse/lisbon_metro_buildings_height.geojson") |>
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
    pre1945     = sum(buildings_pre1945, na.rm = TRUE),
    population  = sum(population,        na.rm = TRUE),
    pop_area_m2 = sum(pop_area_m2,       na.rm = TRUE),
    youth       = sum(youth,             na.rm = TRUE),
    elderly     = sum(elderly,           na.rm = TRUE),
    women       = sum(women,             na.rm = TRUE)
  ) |>
  full_join(
    volume_with_grid |>
      st_drop_geometry() |>
      group_by(id) |>
      summarise(volume_m3 = sum(volume_m3, na.rm = TRUE)),
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

## GRID LEVEL
buildings_grid <- grid |>
  left_join(master_stats, by = c("id" = "grid_id")) |>
  mutate(
    area_m2 = as.numeric(st_area(geom)),
    volume_density = round(volume_m3 / area_m2, 2)
  )
# mapview(buildings_grid, zcol="volume_density")
buildings_grid = buildings_grid |> st_drop_geometry() |> select(id, pre1945, volume_m3, volume_density)
write.csv(buildings_grid, "/data/IMPT/landuse/buildings_grid.csv", row.names = FALSE)

## FREGUESIA LEVEL
buildings_freguesias <- freguesias_geo |>
  select(dtmnfr, freguesia) |> # Keep only essential columns
  left_join(
    master_stats |>
      group_by(freg_id) |>
      summarise(
        total_pre1945 = sum_na(pre1945),
        total_volume_m3 = sum_na(volume_m3)
      ),
    by = c("dtmnfr" = "freg_id")
  ) |>
  mutate(
    total_volume_m3 = round(total_volume_m3),
    area_m2 = round(as.numeric(st_area(geom))),
    volume_density = round(total_volume_m3 / area_m2, 2)
  ) |> 
  rename(freg_id = dtmnfr,
         pre1945 = total_pre1945,
         volume_m3 = total_volume_m3)

# mapview(buildings_freguesias, zcol="volume_density")
buildings_freguesias = buildings_freguesias |> st_drop_geometry() |> select(freg_id, pre1945, volume_m3, volume_density)
write.csv(buildings_freguesias, "/data/IMPT/landuse/buildings_freguesias.csv", row.names = FALSE)

## MUNICIPALITY LEVEL
buildings_municipios <- municipios_geo |>
  select(mun_id, municipio) |>
  left_join(
    master_stats |>
      group_by(mun_id) |>
      summarise(
        total_pre1945 = sum_na(pre1945),
        total_volume_m3 = sum_na(volume_m3)
      ),
    by = "mun_id"
  ) |>
  mutate(
    total_volume_m3 = round(total_volume_m3),
    area_m2 = round(as.numeric(st_area(geom))),
    volume_density = round(total_volume_m3 / area_m2, 2)
  ) |> 
  rename(pre1945 = total_pre1945,
         volume_m3 = total_volume_m3)

# mapview(buildings_municipios, zcol="volume_density")
buildings_municipios = buildings_municipios |> st_drop_geometry() |> select(mun_id, pre1945, volume_m3, volume_density)
write.csv(buildings_municipios, "/data/IMPT/landuse/buildings_municipios.csv", row.names = FALSE)



# Census Data -------------------------------------------------------------
# Derived variables:
#   population_density  = population / grid cell area (pop/km²)
#   youth_ratio         = share aged 0–14
#   elderly_ratio       = share aged 65+
#   women_percentage    = share female

## GRID LEVEL
census_grid <- grid |>
  left_join(master_stats, by = c("id" = "grid_id")) |>
  mutate(
    area_m2            = as.numeric(st_area(geom)),
    population_density = round(population / (area_m2 / 1e6), 1),   # pop/km²
    youth_ratio        = round(youth / population * 100, 2),
    elderly_ratio      = round(elderly / population *100, 2),
    women_percentage   = round(women / population * 100, 2)
  )
# mapview(census_grid, zcol="population_density")
census_grid = census_grid |> st_drop_geometry() |> select(id, population, population_density, youth_ratio, elderly_ratio, women_percentage)
write.csv(census_grid, "/data/IMPT/landuse/census_grid.csv", row.names = FALSE)

## FREGUESIA LEVEL
census_freguesias <- freguesias_geo |>
  select(dtmnfr, freguesia) |>
  left_join(
    master_stats |>
      group_by(freg_id) |>
      summarise(
        population  = sum_na(population),
        youth       = sum_na(youth),
        elderly     = sum_na(elderly),
        women       = sum_na(women)
      ),
    by = c("dtmnfr" = "freg_id")
  ) |>
  mutate(
    area_m2            = round(as.numeric(st_area(geom))),
    population_density = round(population / (area_m2 / 1e6), 1),
    youth_ratio        = round(youth / population * 100, 2),
    elderly_ratio      = round(elderly / population *100, 2),
    women_percentage   = round(women / population * 100, 2)
  ) |> 
  rename(freg_id = dtmnfr)

mapview(census_freguesias, zcol="population_density")
census_freguesias = census_freguesias |> st_drop_geometry() |> select(freg_id, population, population_density, youth_ratio, elderly_ratio, women_percentage)
write.csv(census_freguesias, "/data/IMPT/landuse/census_freguesias.csv", row.names = FALSE)


## MUNICIPALITY LEVEL
census_municipios <- municipios_geo |>
  select(mun_id, municipio) |>
  left_join(
    master_stats |>
      group_by(mun_id) |>
      summarise(
        population  = sum_na(population),
        youth       = sum_na(youth),
        elderly     = sum_na(elderly),
        women       = sum_na(women)
      ),
    by = "mun_id"
  ) |>
  mutate(
    area_m2            = round(as.numeric(st_area(geom))),
    population_density = round(population / (area_m2 / 1e6), 1),
    youth_ratio        = round(youth / population * 100, 2),
    elderly_ratio      = round(elderly / population *100, 2),
    women_percentage   = round(women / population * 100, 2)
  ) 
# mapview(census_municipios, zcol="population_density")
census_municipios = census_municipios |> st_drop_geometry() |> select(mun_id, population, population_density, youth_ratio, elderly_ratio, women_percentage)
write.csv(census_municipios, "/data/IMPT/landuse/census_municipios.csv", row.names = FALSE)



# TO-DO !!!!: understand this:
sum(census_freguesias$population) #2850177
sum(census_municipios$population) #2850177
sum(census_pts$population)  #2870208
# maybe it is better to group_by and summarize at the original census data for freguesias and municipios!




# Pedestrian and Cycling infrastructure -----------------------------------


