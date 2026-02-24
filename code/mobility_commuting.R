# Data loading  -------------------------------------------------

output_dir = "mobility_commuting"

# ODs
trips_freguesias_2024 = readRDS_remote(IMPT_URL("/trips/TRIPSmode_freguesias_2024.Rds"))
od_freguesias_jittered200 = st_read(IMPT_URL("/trips/od_freguesias_jittered_2024.gpkg")) # lines
od_freguesias_jittered_OR_geo = st_read(IMPT_URL("/trips/od_freguesias_jittered200_OR.gpkg")) # points origin
od_freguesias_jittered_DE_geo = st_read(IMPT_URL("/trips/od_freguesias_jittered200_DE.gpkg")) # points destination

# ttms 
ttm_root = "/ttm/ttm_h3_res8"
ttm_list = list(
  # Mode, Travel time matrix file, Time cutoffs to consider
  list("walk", "ttm_walk_120min_202602040800.rds"),
  list("bike", "ttm_bicycle_120min_202602040800.rds"),
  list("car", "ttm_car_120min_202602040800.rds"),
  list("transit_1t", "ttm_transit_120min_202602040800_1transfers.rds"),
  list("transit_2t", "ttm_transit_120min_202602040800_2transfers.rds")
)


# Calculations  -------------------------------------------------
jittering = od_freguesias_jittered200 |> st_drop_geometry()

jittering_origins_grid = st_join(od_freguesias_jittered_OR_geo, grid, join = st_intersects) |> 
  select(id = id.x, id_grid_origin = id.y)
# mapview(grid) + mapview(jittering_origins_grid)
jittering_destinations_grid = st_join(od_freguesias_jittered_DE_geo, grid, join = st_intersects) |> 
  select(id = id.x, id_grid_destination = id.y)

jittering_grid = jittering |> 
  left_join(jittering_origins_grid |> st_drop_geometry(), by = "id") |> 
  left_join(jittering_destinations_grid |> st_drop_geometry(), by = "id")

for (i in seq_along(ttm_list)) {
  mode = ttm_list[[i]][[1]]
  ttm_file = ttm_list[[i]][[2]]
  message(sprintf("Processing mode: %s", mode))
  
  # Load the travel time matrix
  ttm = readRDS_remote(IMPT_URL(paste0(ttm_root, "/", ttm_file))) |> 
    mutate(from_id = as.integer(from_id), to_id = as.integer(to_id))
  
  tt_var = paste("tt", mode, sep = "_")
  
  jittering_grid = jittering_grid |>
    left_join(ttm, by = c("id_grid_origin" = "from_id", "id_grid_destination" = "to_id")) |>
    rename(!!tt_var := travel_time_p50)
  
  message(sprintf("> Finished processing mode! Proceeding..."))
}

jittering_grid = jittering_grid |>
  mutate(
    tt_total_walk = ifelse(is.na(tt_walk), NA, tt_walk * Walk),
    tt_total_bike = ifelse(is.na(tt_bike), NA, tt_bike * Bike),
    tt_total_car = ifelse(is.na(tt_car), NA, tt_car * Car),
    tt_total_transit_1t = ifelse(is.na(tt_transit_1t), NA, tt_transit_1t * PTransit),
    tt_total_transit_2t = ifelse(is.na(tt_transit_2t), NA, tt_transit_2t * PTransit)
  )

write.csv(jittering_grid, IMPT_URL(sprintf("%s/jittering_grid.csv", output_dir)))


# Aggregate by parish and municipality  -------------------------------------------------
grid_commuting = jittering_grid |> 
  group_by(id_grid_origin) |> 
  summarise(
    # Total trips 
    across(c(Walk, Bike, Car, PTransit), ~ sum(.x, na.rm = TRUE)),
    # Total travel time
    across(starts_with("tt_total"), ~ sum(.x, na.rm = TRUE))
  ) |> 
  ungroup() |> 
  mutate(
    avg_tt_walk = ifelse(Walk > 0, tt_total_walk / Walk, NA),
    avg_tt_bike = ifelse(Bike > 0, tt_total_bike / Bike, NA),
    avg_tt_car = ifelse(Car > 0, tt_total_car / Car, NA),
    avg_tt_transit_1t = ifelse(PTransit > 0, tt_total_transit_1t / PTransit, NA),
    avg_tt_transit_2t = ifelse(PTransit > 0, tt_total_transit_2t / PTransit, NA)
  )

grid_commuting_sf = grid |> select(id, geom) |> left_join(grid_commuting, by=c("id" = "id_grid_origin"))
# mapview(grid_commuting_sf, zcol = "Walk")
# mapview(grid_commuting_sf, zcol = "tt_total_walk")
# mapview(grid_commuting_sf, zcol = "avg_tt_walk")
# mapview(grid_commuting_sf, zcol = "avg_tt_car")
# mapview(grid_commuting_sf, zcol = "tt_total_transit_2t")
# mapview(grid_commuting_sf, zcol = "avg_tt_transit_2t")

freguesia_commuting = jittering_grid |> 
  group_by(Origin_dicofre24) |> 
  summarise(
    # Total trips 
    across(c(Walk, Bike, Car, PTransit), ~ sum(.x, na.rm = TRUE)),
    # Total travel time
    across(starts_with("tt_total"), ~ sum(.x, na.rm = TRUE))
  ) |> 
  ungroup() |>
  mutate(
    avg_tt_walk = ifelse(Walk > 0, tt_total_walk / Walk, NA),
    avg_tt_bike = ifelse(Bike > 0, tt_total_bike / Bike, NA),
    avg_tt_car = ifelse(Car > 0, tt_total_car / Car, NA),
    avg_tt_transit_1t = ifelse(PTransit > 0, tt_total_transit_1t / PTransit, NA),
    avg_tt_transit_2t = ifelse(PTransit > 0, tt_total_transit_2t / PTransit, NA)
  )

freguesia_commuting_sf = freguesias |> select(dtmnfr, geom) |> left_join(freguesia_commuting, by=c("dtmnfr" = "Origin_dicofre24"))
# mapview(freguesia_commuting_sf, zcol = "Walk")
# mapview(freguesia_commuting_sf, zcol = "tt_total_walk")
# mapview(freguesia_commuting_sf, zcol = "avg_tt_walk")
# mapview(freguesia_commuting_sf, zcol = "avg_tt_car")
# mapview(freguesia_commuting_sf, zcol = "tt_total_transit_2t")
# mapview(freguesia_commuting_sf, zcol = "PTransit")
# mapview(freguesia_commuting_sf, zcol = "avg_tt_transit_2t")

municipio_commuting = freguesia_commuting |>
  left_join(freguesias |> select(dtmnfr, municipio), by=c("Origin_dicofre24" = "dtmnfr")) |>
  group_by(municipio) |> 
  summarise(
    # Total trips
    across(c(Walk, Bike, Car, PTransit), ~ sum(.x, na.rm = TRUE)),
    # Total travel time
    across(starts_with("tt_total"), ~ sum(.x, na.rm = TRUE))
  ) |> 
  ungroup() |>
  mutate(
    avg_tt_walk = ifelse(Walk > 0, tt_total_walk / Walk, NA),
    avg_tt_bike = ifelse(Bike > 0, tt_total_bike / Bike, NA),
    avg_tt_car = ifelse(Car > 0, tt_total_car / Car, NA),
    avg_tt_transit_1t = ifelse(PTransit > 0, tt_total_transit_1t / PTransit, NA),
    avg_tt_transit_2t = ifelse(PTransit > 0, tt_total_transit_2t / PTransit, NA)
  )

municipio_commuting_sf = municipios |> select(municipio, geom) |> left_join(municipio_commuting, by=c("municipio" = "municipio"))
# mapview(municipio_commuting_sf, zcol = "avg_tt_car")
# mapview(municipio_commuting_sf, zcol = "avg_tt_walk")
# mapview(municipio_commuting_sf, zcol = "avg_tt_transit_1t")
# mapview(municipio_commuting_sf, zcol = "avg_tt_transit_2t")


st_write(grid_commuting_sf, IMPT_URL(sprintf("%s/grid_commuting.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(grid_commuting, IMPT_URL(sprintf("%s/grid_commuting.csv", output_dir)), row.names = FALSE) 
st_write(freguesia_commuting_sf, IMPT_URL(sprintf("%s/freguesia_commuting.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(freguesia_commuting, IMPT_URL(sprintf("%s/freguesia_commuting.csv", output_dir)), row.names = FALSE) 
st_write(municipio_commuting_sf, IMPT_URL(sprintf("%s/municipio_commuting.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(municipio_commuting, IMPT_URL(sprintf("%s/municipio_commuting.csv", output_dir)), row.names = FALSE)
