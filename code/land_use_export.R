# export land use auxiliary variables

# load libraries
library(tidyverse)
library(sf)

# Helper: sum that returns NA when ALL inputs are NA (unlike sum(na.rm=TRUE) which returns 0)
sum_na = function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# Load Reference Data --------------------------------------------------
grid = st_read("/data/IMPT/geo/grelha_h3_r8.gpkg") |> mutate(id = as.character(id))
grid_freg_mun = read.csv("useful_data/grid_nuts.csv") |> mutate(grid_id = as.character(grid_id), freg_id = as.character(freg_id), mun_id = as.character(mun_id))
freguesias_geo = st_read("useful_data/freguesias.gpkg")
municipios_geo = st_read("/data/IMPT/geo/municipios_2024.gpkg")
municipios_id = read.csv("useful_data/municipios_id.csv") 
municipios_geo = municipios_geo |> left_join(municipios_id) |> mutate(mun_id = as.character(mun_id))


# Houses and buildings ----------------------------------------------------
census_pts = st_read("/data/IMPT/geo/census24_points.gpkg") |> 
  select(buildings_pre1945 = N_EDIFICIOS_CONSTR_ANTES_1945)
volume_pts = st_read("/data/IMPT/landuse/lisbon_metro_buildings_height.geojson") |> 
  select(volume_m3)


# Assign every point to a Grid ID
census_with_grid = st_join(census_pts, grid["id"], join = st_within)
volume_with_grid = st_join(volume_pts, grid["id"], join = st_within)

# Create Master Tabular Summaries 
# Aggregate by grid_id first
grid_stats = census_with_grid |>
  st_drop_geometry() |>
  group_by(id) |>
  summarise(pre1945 = sum(buildings_pre1945, na.rm = TRUE)) |>
  full_join(
    volume_with_grid |> 
      st_drop_geometry() |> 
      group_by(id) |> 
      summarise(vol_m3 = sum(volume_m3, na.rm = TRUE)),
    by = "id"
  ) |>
  rename(grid_id = id)

# Join the cross-reference (freg_id, mun_id) to these stats
# Grid cells with no census centroid inside them keep NA (no false zeros)
master_stats = grid_freg_mun |>
  left_join(grid_stats |> mutate(grid_id = as.character(grid_id)), by = "grid_id") |>
  mutate(
    grid_id = as.character(grid_id),
    freg_id = as.character(freg_id),
    mun_id = as.character(mun_id))

# Export Levels 

## GRID LEVEL 
buildings_grid = grid |>
  left_join(master_stats, by = c("id" = "grid_id")) |>
  mutate(
    area_m2 = as.numeric(st_area(geom)),
    volume_density = round(vol_m3 / area_m2, 2)
  )
# mapview(buildings_grid, zcol="volume_density")
write.csv(st_drop_geometry(buildings_grid), "/data/IMPT/landuse/buildings_grid.csv", row.names = FALSE)

## FREGUESIA LEVEL 
buildings_freguesias = freguesias_geo |>
  select(dtmnfr, freguesia) |> # Keep only essential columns
  left_join(
    master_stats |> 
      group_by(freg_id) |> 
      summarise(
        total_pre1945 = sum_na(pre1945),
        total_volume_m3 = sum_na(vol_m3)
      ), 
    by = c("dtmnfr" = "freg_id")
  ) |>
  mutate(
    total_volume_m3 = round(total_volume_m3),
    area_m2 = round(as.numeric(st_area(geom))),
    volume_density = round(total_volume_m3 / area_m2, 2)
  )
# mapview(buildings_freguesias, zcol="volume_density")
write.csv(buildings_freguesias |> st_drop_geometry(), "/data/IMPT/landuse/buildings_freguesias.csv", row.names = FALSE)

## MUNICIPALITY LEVEL
buildings_municipios = municipios_geo |>
  select(mun_id, municipio) |> 
  left_join(
    master_stats |> 
      group_by(mun_id) |> 
      summarise(
        total_pre1945 = sum_na(pre1945),
        total_volume_m3 = sum_na(vol_m3)
      ), 
    by = "mun_id"
  ) |>
  mutate(
    total_volume_m3 = round(total_volume_m3),
    area_m2 = round(as.numeric(st_area(geom))),
    volume_density = round(total_volume_m3 / area_m2, 2)
  )
# mapview(buildings_municipios, zcol="volume_density")
write.csv(buildings_municipios |> st_drop_geometry(), "/data/IMPT/landuse/buildings_municipios.csv", row.names = FALSE)



# Pedestrian and Cycling infrastructure -----------------------------------



