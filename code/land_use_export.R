# export land use auxiliary variables

# load libraries
library(tidyverse)
library(sf)


# Load Reference Data --------------------------------------------------
# population and census from original census 21
census24_fregmun = read.csv(, "useful_data/census24_fregmun.csv", row.names = FALSE)

grid <- st_read("/data/IMPT/geo/grelha_h3_r8.gpkg") |> mutate(id = as.character(id))
grid_freg_mun <- read.csv("useful_data/grid_nuts.csv") |> mutate(grid_id = as.character(grid_id), freg_id = as.character(freg_id), mun_id = as.character(mun_id))
freguesias_geo <- st_read("useful_data/freguesias.gpkg")
freguesias_area = freguesias_geo |>
  st_drop_geometry() |>
  mutate(area_m2 = area_ha * 10000,
         area_km2 = round(area_ha / 100, 1))

mun_freg_id = census24_fregmun |>
  select(freg_id, mun_id) |>
  unique()
municipios_area = freguesias_area |>
  left_join(mun_freg_id, by = c("dtmnfr" = "freg_id")) |>
  group_by(mun_id, municipio) |>
  summarize(
    area_m2 = sum(area_m2),
    area_km2 = sum(area_km2)) |>
  ungroup()

# Helper: sum that returns NA when ALL inputs are NA (unlike sum(na.rm=TRUE) which returns 0)
sum_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# Census, Houses and buildings ----------------------------------------------------
census_pts = st_read("/data/IMPT/geo/census24_points.gpkg") |> 
  select(
    freg_id = dicofre24,
    buildings           = N_EDIFICIOS_CLASSICOS,
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
    buildings_pre1945     = sum(buildings_pre1945, na.rm = TRUE),
    buildings            = sum(buildings,        na.rm = TRUE),
    population  = sum(population,        na.rm = TRUE),
    population_density = sum(pop_area_m2,       na.rm = TRUE),
    youth       = sum(youth,             na.rm = TRUE),
    elderly     = sum(elderly,           na.rm = TRUE),
    women       = sum(women,             na.rm = TRUE)
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

## GRID LEVEL
landuse_grid <- grid |>
  left_join(master_stats, by = c("id" = "grid_id")) |>
  mutate(
    area_m2 = as.numeric(st_area(geom)),
    volume_density = round(buildings_volume_m3 / area_m2, 2),
    population_density = round(population / (area_m2 / 1e6), 1),   # pop/km²
    youth_ratio        = round(youth / population * 100, 2),
    elderly_ratio      = round(elderly / population *100, 2),
    women_percentage   = round(women / population * 100, 2)
  ) |> 
  mutate(area_km2 = round(area_m2 / 1e6, 2),
         buildings_pre1945_percentage = round(buildings_pre1945 / buildings * 100, 2)
  )
# mapview(landuse_grid, zcol="volume_density")
landuse_grid = landuse_grid |> st_drop_geometry() |> select(id, area_km2,
                                                            buildings, buildings_pre1945, buildings_pre1945_percentage,
                                                            buildings_volume_m3, volume_density,
                                                            population, population_density, youth_ratio, elderly_ratio, women_percentage)
write.csv(landuse_grid, "/data/IMPT/landuse/landuse_grid.csv", row.names = FALSE)



## FREGUESIA LEVEL
landuse_freguesias <- census24_fregmun |>
  left_join(
    master_stats |>
      select(freg_id, volume_m3) |>
      group_by(freg_id) |>
      summarise(
        buildings_volume_m3 = sum_na(volume_m3)
      )
  ) |>
  left_join(freguesias_area |> select(dtmnfr, area_m2, area_km2), by = c("freg_id" = "dtmnfr")) |>
  mutate(
    buildings_volume_m3 = round(buildings_volume_m3),
    volume_density = round(buildings_volume_m3 / area_m2, 2),
    buildings_pre1945_percentage = round(buildings_pre1945 / buildings * 100, 2)
  ) |> 
  mutate(
          population_density = round(population / area_km2, 1),   # pop/km²
          youth_ratio        = round(youth / population * 100, 2),
          elderly_ratio      = round(elderly / population *100, 2),
          women_percentage   = round(women / population * 100, 2)
          )

landuse_freguesias = landuse_freguesias |> select(freg_id, area_km2,
                                                  buildings, buildings_pre1945, buildings_pre1945_percentage,
                                                  buildings_volume_m3, volume_density,
                                                  population, population_density, youth_ratio, elderly_ratio, women_percentage)
write.csv(landuse_freguesias, "/data/IMPT/landuse/landuse_freguesias.csv", row.names = FALSE)

## MUNICIPALITY LEVEL
landuse_municipios <- census24_fregmun |>
  left_join(
    master_stats |>
      select(freg_id, volume_m3) |>
      group_by(freg_id) |>
      summarise(
        buildings_volume_m3 = sum_na(volume_m3)
      ) |> 
      ungroup()
  ) |>
  group_by(mun_id) |>
  summarise(
    population = sum(population, na.rm = TRUE),
    youth = sum(youth, na.rm = TRUE),
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
    population_density = round(population / area_km2, 1),   # pop/km²
    youth_ratio        = round(youth / population * 100, 2),
    elderly_ratio      = round(elderly / population *100, 2),
    women_percentage   = round(women / population * 100, 2)
  )

landuse_municipios = landuse_municipios |> select(mun_id, area_km2,
                                                  buildings, buildings_pre1945, buildings_pre1945_percentage,
                                                  buildings_volume_m3, volume_density,
                                                  population, population_density, youth_ratio, elderly_ratio, women_percentage)
write.csv(landuse_municipios, "/data/IMPT/landuse/landuse_municipios.csv", row.names = FALSE)


# Final check
sum(census_pts$population)  #2870208
sum(landuse_grid$population, na.rm = TRUE) #2850646 # some census bgri centroids fall outside the grid cells, so we lose some 1450 population
sum(landuse_municipios$population) #2870208
sum(landuse_freguesias$population) #2870208



# Pedestrian and Cycling infrastructure -----------------------------------


