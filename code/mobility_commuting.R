# Data loading  -------------------------------------------------

output_dir = "mobility_commuting"

# ODs
od_freguesias_jittered200 = st_read(IMPT_URL("/trips/od_freguesias_jittered_2024.gpkg")) # lines
nrow(od_freguesias_jittered200) # 31891
sum(od_freguesias_jittered200$Total) # 5299853
od_freguesias_jittered50 = st_read(IMPT_URL("/trips/od_jobs_jt50_buildings.gpkg")) # lines
nrow(od_freguesias_jittered50) # 19547
sum(od_freguesias_jittered50$trips) # 839142.1

od_freguesias_jittered_OR_geo = st_read(IMPT_URL("/trips/od_jobs_jt50_buildings_OR.gpkg")) # points origin
# mapview(od_freguesias_jittered_OR_geo)
od_freguesias_jittered_DE_geo = st_read(IMPT_URL("/trips/od_jobs_jt50_buildings_DE.gpkg")) # points destination

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
jittering = od_freguesias_jittered50 |> st_drop_geometry()

jittering_origins_grid = st_join(od_freguesias_jittered_OR_geo, grid, join = st_intersects) |> 
  select(id = id.x, id_grid_origin = id.y)
# mapview(grid) + mapview(jittering_origins_grid)
jittering_destinations_grid = st_join(od_freguesias_jittered_DE_geo, grid, join = st_intersects) |> 
  select(id = id.x, id_grid_destination = id.y)

jittering_grid = jittering |> 
  left_join(jittering_origins_grid |> st_drop_geometry(), by = "id") |> 
  left_join(jittering_destinations_grid |> st_drop_geometry(), by = "id") |>
  filter(!is.na(id_grid_origin) & !is.na(id_grid_destination))

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
    # Replace NAs with max value of all other rows
    tt_walk = ifelse(is.na(tt_walk), max(tt_walk, na.rm = TRUE), tt_walk),
    tt_bike = ifelse(is.na(tt_bike), max(tt_bike, na.rm = TRUE), tt_bike),
    tt_car = ifelse(is.na(tt_car), max(tt_car, na.rm = TRUE), tt_car),
    tt_transit_1t = ifelse(is.na(tt_transit_1t), max(tt_transit_1t, na.rm = TRUE), tt_transit_1t),
    tt_transit_2t = ifelse(is.na(tt_transit_2t), max(tt_transit_2t, na.rm = TRUE), tt_transit_2t),
    # Compute total travel time by multiplying the travel time by the number of trips for each mode
    tt_total_walk = ifelse(is.na(tt_walk), NA, tt_walk * trips),
    tt_total_bike = ifelse(is.na(tt_bike), NA, tt_bike * trips),
    tt_total_car = ifelse(is.na(tt_car), NA, tt_car * trips),
    tt_total_transit_1t = ifelse(is.na(tt_transit_1t), NA, tt_transit_1t * trips),
    tt_total_transit_2t = ifelse(is.na(tt_transit_2t), NA, tt_transit_2t * trips)
  )

write.csv(jittering_grid, IMPT_URL(sprintf("%s/jittering_grid.csv", output_dir)))


# Aggregate by parish and municipality  -------------------------------------------------
aggregated_commuting_for_geometry = function(grid) {
  return (
    grid |> 
      summarise(
        # Count number of grids aggregated
        nr_grids = n(),
        # Total trips 
        trips = sum(trips),
        # Total travel time
        across(starts_with("tt_total"),~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE))
      ) |> 
      ungroup() |> 
      mutate(
        # Compute average time for each mode
        avg_tt_walk = ifelse(is.na(tt_total_walk), NA, round(tt_total_walk / trips, digits=2)),
        avg_tt_bike = ifelse(is.na(tt_total_bike), NA, round(tt_total_bike / trips, digits=2)), 
        avg_tt_car = ifelse(is.na(tt_total_car), NA, round(tt_total_car / trips, digits=2)), 
        avg_tt_transit_1t = ifelse(is.na(tt_total_transit_1t), NA, round(tt_total_transit_1t / trips, digits=2)),
        avg_tt_transit_2t = ifelse(is.na(tt_total_transit_2t), NA, round(tt_total_transit_2t / trips, digits=2)),
        # Compute aggregated time for 
        across(starts_with("tt_total"), ~ round(.x, digits=2))
      )
  )
}

grid_commuting = aggregated_commuting_for_geometry(jittering_grid |> group_by(id_grid_origin))
summary(grid_commuting)

grid_commuting_sf = grid |> select(id, geom) |> left_join(grid_commuting, by=c("id" = "id_grid_origin"))
# mapview(grid_commuting_sf, zcol = "trips")
# mapview(grid_commuting_sf, zcol = "tt_total_walk")
# mapview(grid_commuting_sf, zcol = "avg_tt_walk")
# mapview(grid_commuting_sf, zcol = "avg_tt_car")
# mapview(grid_commuting_sf, zcol = "tt_total_transit_2t")
# mapview(grid_commuting_sf, zcol = "avg_tt_transit_2t")
# mapview(grid_commuting_sf, zcol = "avg_tt_transit_1t")
# mapview(grid_commuting_sf, zcol = "nr_grids_na_walk")

freguesia_commuting = aggregated_commuting_for_geometry(jittering_grid |> group_by(Origin_dicofre24))

freguesia_commuting_sf = freguesias |> select(dtmnfr, geom) |> left_join(freguesia_commuting, by=c("dtmnfr" = "Origin_dicofre24"))
# mapview(freguesia_commuting_sf, zcol = "trips")
# mapview(freguesia_commuting_sf, zcol = "avg_tt_walk")
# mapview(freguesia_commuting_sf, zcol = "avg_tt_car")
# mapview(freguesia_commuting_sf, zcol = "tt_total_transit_2t")
# mapview(freguesia_commuting_sf, zcol = "PTransit")
# mapview(freguesia_commuting_sf, zcol = "avg_tt_transit_2t")
# mapview(freguesia_commuting_sf, zcol = "nr_grids_na_walk")

municipio_commuting = aggregated_commuting_for_geometry(
  jittering_grid |>
    left_join(freguesias |> select(dtmnfr, municipio), by=c("Origin_dicofre24" = "dtmnfr")) |>
    group_by(municipio) 
)
municipio_commuting_sf = municipios |> select(municipio, geom) |> left_join(municipio_commuting, by=c("municipio" = "municipio"))
# mapview(municipio_commuting_sf, zcol = "trips")
# mapview(municipio_commuting_sf, zcol = "avg_tt_car")
# mapview(municipio_commuting_sf, zcol = "avg_tt_walk")
# mapview(municipio_commuting_sf, zcol = "weighted_tt_walk")
# mapview(municipio_commuting_sf, zcol = "avg_tt_transit_1t")
# mapview(municipio_commuting_sf, zcol = "avg_tt_transit_2t")
# mapview(municipio_commuting_sf, zcol = "nr_grids_na_walk")


st_write(grid_commuting_sf, IMPT_URL(sprintf("%s/grid_commuting.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(grid_commuting, IMPT_URL(sprintf("%s/grid_commuting.csv", output_dir)), row.names = FALSE) 
st_write(freguesia_commuting_sf, IMPT_URL(sprintf("%s/freguesia_commuting.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(freguesia_commuting, IMPT_URL(sprintf("%s/freguesia_commuting.csv", output_dir)), row.names = FALSE) 
st_write(municipio_commuting_sf, IMPT_URL(sprintf("%s/municipio_commuting.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(municipio_commuting, IMPT_URL(sprintf("%s/municipio_commuting.csv", output_dir)), row.names = FALSE)
