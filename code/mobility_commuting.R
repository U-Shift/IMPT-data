# Travel time (peak) + Number of transfers for key destinations (transit)
# Purpose     Determine the travel time for commuting, at peak hours, by different modes, and the number of transfers for transit
# Scale       hex, parish, municipality
# Issue       -


# I. Commuting travel time (peak)  -------------------------------------------------
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
nrow(od_freguesias_jittered_OR_geo)
od_freguesias_jittered_DE_geo = st_read(IMPT_URL("/trips/od_jobs_jt50_buildings_DE.gpkg")) # points destination
nrow(od_freguesias_jittered_DE_geo)

# Fill transparent, only border, tick
# mapview(grid) +
# mapview(municipios, col.regions="grey", alpha.regions=0.1, layer.name="Municipios") +
#   mapview(freguesias, col.regions="grey", alpha.regions=0.1, layer.name="Freguesias") +
#  mapview(od_freguesias_jittered_OR_geo, color="red") + mapview(od_freguesias_jittered_DE_geo, color="blue")

# ttms 
ttm_list = list(
  # Mode, Travel time matrix file, Time cutoffs to consider
  list("walk", "/ttm/ttm_h3_res8/ttm_walk_120min_202602040800.rds"),
  list("bike", "/ttm/ttm_h3_res8/ttm_bicycle_120min_202602040800.rds"),
  list("car", "/ttm/ttm_h3_res8/ttm_car_120min_202602040800.rds"),
  list("transit_0t_120m_15w", "/ttm/ttm_h3_res8/ttm_transit_120min_202602040800_0transfers.rds"),
  list("transit_1t_120m_15w", "/ttm/ttm_h3_res8/ttm_transit_120min_202602040800_1transfers.rds"),
  list("transit_2t_120m_15w", "/ttm/ttm_h3_res8/ttm_transit_120min_202602040800_2transfers.rds"),
  list("transit_3t_120m_15w", "/ttm/ttm_h3_res8/ttm_transit_120min_202602040800_3transfers.rds"),
  list("transit_4t_240m_15w", "/ttm/ttm_h3_res8/ttm_transit_240min_202602040800_4transfers.rds"),
  list("transit_1t_120m_20w", "/ttm/ttm_h3_res8_20minw/ttm_transit_120min_202602040800_1transfers.rds"),
  list("transit_2t_120m_20w", "/ttm/ttm_h3_res8_20minw/ttm_transit_120min_202602040800_2transfers.rds"),
  list("transit_3t_120m_20w", "/ttm/ttm_h3_res8_20minw/ttm_transit_120min_202602040800_3transfers.rds"),
  list("transit_1t_120m_30w", "/ttm/ttm_h3_res8_30minw/ttm_transit_120min_202602040800_1transfers.rds"),
  list("transit_2t_120m_30w", "/ttm/ttm_h3_res8_30minw/ttm_transit_120min_202602040800_2transfers.rds"),
  list("transit_3t_120m_30w", "/ttm/ttm_h3_res8_30minw/ttm_transit_120min_202602040800_3transfers.rds")
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
  ttm = readRDS_remote(IMPT_URL(ttm_file)) |> 
    mutate(from_id = as.integer(from_id), to_id = as.integer(to_id))
  
  tt_var = paste("tt", mode, sep = "_")
  
  jittering_grid = jittering_grid |>
    left_join(ttm, by = c("id_grid_origin" = "from_id", "id_grid_destination" = "to_id")) |>
    rename(!!tt_var := travel_time_p50)
  
  message(sprintf("> Finished processing mode! Proceeding..."))
}
jittering_grid_original = jittering_grid
summary(jittering_grid_original)

jittering_grid = jittering_grid_original |>
  mutate(
    # 1. Create 'na_' flag columns for every 'tt_' column
    across(
      starts_with("tt_"), 
      ~ ifelse(is.na(.x), 1, 0), 
      .names = "na_{sub('tt_', '', .col)}"
    ),
    # 2. Replace NAs in 'tt_' columns with their respective column maximums
    across(
      starts_with("tt_"), 
      ~ ifelse(is.na(.x), max(.x, na.rm = TRUE), .x)
    ),
    # 3. Create 'tt_total_' columns by multiplying by trips
    # Note: Using the original column values (now with max imputation)
    across(
      starts_with("tt_"), 
      ~ .x * trips, 
      .names = "tt_total_{sub('tt_', '', .col)}"
    )
  )

write.csv(jittering_grid, IMPT_URL(sprintf("%s/jittering_grid.csv", output_dir)))


# Aggregate by parish and municipality  -------------------------------------------------
aggregated_commuting_for_geometry = function(grid) {
  return (
    grid |> 
      summarise(
        # 1. Weighted travel time for all tt_ columns
        across(
          starts_with("tt_") & !starts_with("tt_total"), 
          ~ round(weighted.mean(.x, trips, na.rm = TRUE), 2), 
          .names = "avg_{.col}"
        ),
        # Total trips na per mode
        # 2. Total trips where NA flag is 1 for all na_ columns
        across(
          starts_with("na_"), 
          ~ round(sum(ifelse(.x == 1, trips, 0)), 2), 
          .names = "trips_{.col}"
        ),
        # 3. Total trips for the group
        trips = round(sum(trips), digits=2)
      ) |> 
      ungroup() |> 
      mutate(
        # 4. Compute Ratio of trips NA (PNA)
        across(
          starts_with("trips_na_"), 
          ~ ifelse(trips == 0, NA, round(.x / trips, 2)), 
          .names = "{sub('trips_na_', 'PNA_', .col)}"
        )
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
# mapview(grid_commuting_sf, zcol = "tt_total_transit_2t_120m_15w")
# mapview(grid_commuting_sf, zcol = "avg_tt_transit_2t_120m_15w")
# mapview(grid_commuting_sf, zcol = "avg_tt_transit_1t_120m_15w")
# mapview(grid_commuting_sf, zcol = "nr_grids_na_walk")
# mapview(grid_commuting_sf, zcol = "PNA_transit_2t_120m_15w")
# mapview(grid_commuting_sf |> filter(!is.na(trips)), zcol = "PNA_transit_4t_240m_15w")
# mapview(grid_commuting_sf |> filter(!is.na(trips)), zcol = "PNA_car")

freguesia_commuting = aggregated_commuting_for_geometry(jittering_grid |> group_by(Origin_dicofre24))

freguesia_commuting_sf = freguesias |> select(dtmnfr, geom) |> left_join(freguesia_commuting, by=c("dtmnfr" = "Origin_dicofre24"))
# mapview(freguesia_commuting_sf, zcol = "trips")
# mapview(freguesia_commuting_sf, zcol = "avg_tt_walk")
# mapview(freguesia_commuting_sf, zcol = "avg_tt_car")
# mapview(freguesia_commuting_sf, zcol = "tt_total_transit_2t_120m_15w")
# mapview(freguesia_commuting_sf, zcol = "PTransit")
# mapview(freguesia_commuting_sf, zcol = "avg_tt_transit_2t_120m_15w")
# mapview(freguesia_commuting_sf, zcol = "nr_grids_na_walk")
# mapview(freguesia_commuting_sf, zcol = "PNA_transit_2t_120m_15w")
# mapview(freguesia_commuting_sf, zcol = "PNA_transit_4t_240m_15w")

municipio_commuting = aggregated_commuting_for_geometry(
  jittering_grid |>
    left_join(freguesias |> select(dtmnfr, municipio), by=c("Origin_dicofre24" = "dtmnfr")) |>
    group_by(municipio) 
)
municipio_commuting_sf = municipios |> select(municipio, geom) |> left_join(municipio_commuting, by=c("municipio" = "municipio"))
# mapview(municipio_commuting_sf, zcol = "trips")
# mapview(municipio_commuting_sf, zcol = "avg_tt_car")
# mapview(municipio_commuting_sf, zcol = "avg_tt_walk")
# mapview(municipio_commuting_sf, zcol = "PNA_walk")
# mapview(municipio_commuting_sf, zcol = "weighted_tt_walk")
# mapview(municipio_commuting_sf, zcol = "avg_tt_transit_1t_120m_15w")
# mapview(municipio_commuting_sf, zcol = "avg_tt_transit_2t_120m_15w")
# mapview(municipio_commuting_sf, zcol = "nr_grids_na_walk")
# mapview(municipio_commuting_sf, zcol = "PNA_transit_2t_120m_15w")
# mapview(municipio_commuting_sf, zcol = "PNA_transit_2t_120m_30w")
# mapview(municipio_commuting_sf, zcol = "PNA_transit_3t_120m_15w")
# mapview(municipio_commuting_sf, zcol = "PNA_transit_3t_120m_30w")
# mapview(municipio_commuting_sf, zcol = "PNA_transit_4t_240m_15w")


st_write(grid_commuting_sf, IMPT_URL(sprintf("%s/grid_commuting.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(grid_commuting, IMPT_URL(sprintf("%s/grid_commuting.csv", output_dir)), row.names = FALSE) 
st_write(freguesia_commuting_sf, IMPT_URL(sprintf("%s/freguesia_commuting.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(freguesia_commuting, IMPT_URL(sprintf("%s/freguesia_commuting.csv", output_dir)), row.names = FALSE) 
st_write(municipio_commuting_sf, IMPT_URL(sprintf("%s/municipio_commuting.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(municipio_commuting, IMPT_URL(sprintf("%s/municipio_commuting.csv", output_dir)), row.names = FALSE)

# Debug
grid_population # From accessibility.R
grid_debug = grid_commuting_sf |> filter(!is.na(trips)) |> 
  left_join(grid_population |> st_drop_geometry()) |>  # Get census data
  mutate(
    trips_residents_diff = trips - residents
  )
grid_centroids = points |> filter(id %in% grid_debug$id)
points = points_h3 # From data_load.R
pois_transit = st_read(IMPT_URL("/pois/transit_stops.gpkg")) # From data_prep.R
od_jobs_jittered_OR_geo = st_read(IMPT_URL("/trips/od_jobs_jt50_buildings_OR.gpkg")) # From ttm_gridh3.R
od_jobs_jittered_DE_geo = st_read(IMPT_URL("/trips/od_jobs_jt50_buildings_DE.gpkg"))


# Travel Time
mapview(municipios, col.regions="grey", alpha.regions=0.1, layer.name="Municipios", hide=TRUE) +
  mapview(freguesias, col.regions="grey", alpha.regions=0.1, layer.name="Freguesias", hide=TRUE) +
  mapview(freguesia_commuting_sf, zcol = "avg_tt_car", hide=TRUE, layer.name="Freguesia avg tt Car 120min") +
  mapview(freguesia_commuting_sf, zcol = "avg_tt_transit_2t_120m_15w", hide=TRUE, layer.name="Freguesia avg tt Transit 2t 120min 15walk") +
  mapview(freguesia_commuting_sf, zcol = "avg_tt_transit_3t_120m_15w", hide=TRUE, layer.name="Freguesia avg tt Transit 3t 120min 15walk") +
  mapview(freguesia_commuting_sf, zcol = "avg_tt_transit_3t_120m_30w", hide=TRUE, layer.name="Freguesia avg tt Transit 3t 120min 30walk") +
  mapview(freguesia_commuting_sf, zcol = "avg_tt_transit_4t_240m_15w", hide=TRUE, layer.name="Freguesia avg tt Transit 4t 240min 15walk") +
  mapview(municipio_commuting_sf, zcol = "avg_tt_car", hide=TRUE, layer.name="Municipio avg tt Car 120min") +
  mapview(municipio_commuting_sf, zcol = "avg_tt_transit_2t_120m_15w", hide=TRUE, layer.name="Municipio avg tt Transit 2t 120min 15walk") +
  mapview(municipio_commuting_sf, zcol = "avg_tt_transit_3t_120m_15w", hide=TRUE, layer.name="Municipio avg tt Transit 3t 120min 15walk") +
  mapview(municipio_commuting_sf, zcol = "avg_tt_transit_3t_120m_30w", hide=TRUE, layer.name="Municipio avg tt Transit 3t 120min 30walk") +
  mapview(municipio_commuting_sf, zcol = "avg_tt_transit_4t_240m_15w", hide=TRUE, layer.name="Municipio avg tt Transit 4t 240min 15walk") +
  # PNA
  mapview(freguesia_commuting_sf, zcol = "PNA_transit_2t_120m_15w", hide=TRUE, layer.name="Freguesia PNA Transit 2t 120min 15walk") +
  mapview(freguesia_commuting_sf, zcol = "PNA_transit_3t_120m_15w", hide=TRUE, layer.name="Freguesia PNA Transit 3t 120min 15walk") +
  mapview(freguesia_commuting_sf, zcol = "PNA_transit_3t_120m_30w", hide=TRUE, layer.name="Freguesia PNA Transit 3t 120min 30walk") +
  mapview(freguesia_commuting_sf, zcol = "PNA_transit_4t_240m_15w", hide=TRUE, layer.name="Freguesia PNA Transit 4t 240min 15walk") +
  mapview(freguesia_commuting_sf, zcol = "PNA_car", hide=TRUE, layer.name="Freguesia PNA Car 120min") +
  mapview(municipio_commuting_sf, zcol = "PNA_transit_2t_120m_15w", hide=TRUE, layer.name="Municipio PNA Transit 2t 120min 15walk") +
  mapview(municipio_commuting_sf, zcol = "PNA_transit_3t_120m_15w", hide=TRUE, layer.name="Municipio PNA Transit 3t 120min 15walk") +
  mapview(municipio_commuting_sf, zcol = "PNA_transit_3t_120m_30w", hide=TRUE, layer.name="Municipio PNA Transit 3t 120min 30walk") +
  mapview(municipio_commuting_sf, zcol = "PNA_transit_4t_240m_15w", hide=TRUE, layer.name="Municipio PNA Transit 4t 240min 15walk") + 
  mapview(municipio_commuting_sf, zcol = "PNA_car", hide=TRUE, layer.name="Municipio PNA Car 120min") +
  # Grid meta
  mapview(grid_debug, zcol="trips", hide=TRUE, layer.name="Grid trips") +
  mapview(grid_debug, zcol="residents", hide=TRUE, layer.name="Grid residents") +
  mapview(grid_debug, zcol="trips_residents_diff", hide=TRUE, layer.name="Grid trips - residents") +
  mapview(grid_debug |> filter(trips_residents_diff>=0), zcol="trips_residents_diff", hide=TRUE, layer.name="Grid trips - residents POSITIVE") +
  mapview(grid_debug |> filter(trips_residents_diff<0), zcol="trips_residents_diff", hide=TRUE, layer.name="Grid trips - residents NEGATIVE") +
  # PNA Grid
  mapview(grid_debug, zcol="PNA_car", hide=TRUE, layer.name="Grid PNA Car 120min") + # Grid with car PNA
  mapview(grid_debug, zcol="PNA_transit_2t_120m_15w", hide=TRUE, layer.name="Grid PNA Transit 2t 120min 15walk") + # Grid with PNA transit
  mapview(grid_debug, zcol="PNA_transit_3t_120m_15w", hide=TRUE, layer.name="Grid PNA Transit 3t 120min 15walk") + # Grid with PNA transit
  mapview(grid_debug, zcol="PNA_transit_3t_120m_30w", hide=TRUE, layer.name="Grid PNA Transit 3t 120min 30walk") + # Grid with PNA transit
  mapview(grid_debug, zcol="PNA_transit_4t_240m_15w", layer.name="Grid PNA Transit 4t 240min 15walk") + # Grid with PNA transit
  mapview(grid_centroids, col.regions="orange", hide=TRUE, layer.name="Grid centroids") + # Grid centroids (used for ttm calculations)
  # POIs and jittering
  mapview(pois_transit, col.regions="black", layer.name="Transit stops") + # Transit stops (from OSM, used in r5r network)
  mapview(od_jobs_jittered_OR_geo, col.regions="blue", zcol=NULL, hide=TRUE, layer.name="Jittering origins") + 
  mapview(od_jobs_jittered_DE_geo, col.regions="red", zcol=NULL, hide=TRUE, layer.name="Jittering destinations")

# II. Congestion related travel time (car)  -------------------------------------------------

ttm_root = "/ttm/ttm_h3_res8"
ttm_peak = readRDS_remote(IMPT_URL(paste0(ttm_root, "/ttm_car_120min_202602040800.rds")))
ttm_dawn = readRDS_remote(IMPT_URL(paste0(ttm_root, "/ttm_car_120min_202602040300.rds")))
ttm_weekend = readRDS_remote(IMPT_URL(paste0(ttm_root, "/ttm_car_120min_202602082000.rds")))

summary(ttm_peak)
summary(ttm_dawn)
summary(ttm_weekend)


# III. Number of transfers for key destinations (transit) -------------------------------------------------

ttm_root = "/ttm/ttm_h3_res8"
ttm_0t = readRDS_remote(IMPT_URL(paste0(ttm_root, "/ttm_transit_120min_202602040800_0transfers.rds"))) |>
  mutate(from_id = as.integer(from_id), to_id = as.integer(to_id))
ttm_1t = readRDS_remote(IMPT_URL(paste0(ttm_root, "/ttm_transit_120min_202602040800_1transfers.rds"))) |>
  mutate(from_id = as.integer(from_id), to_id = as.integer(to_id))
ttm_2t = readRDS_remote(IMPT_URL(paste0(ttm_root, "/ttm_transit_120min_202602040800_2transfers.rds"))) |>
  mutate(from_id = as.integer(from_id), to_id = as.integer(to_id))
ttm_3t = readRDS_remote(IMPT_URL(paste0(ttm_root, "/ttm_transit_120min_202602040800_3transfers.rds"))) |>
  mutate(from_id = as.integer(from_id), to_id = as.integer(to_id))
ttm_4t = readRDS_remote(IMPT_URL(paste0(ttm_root, "/ttm_transit_120min_202602040800_4transfers.rds"))) |>
  mutate(from_id = as.integer(from_id), to_id = as.integer(to_id))
summary(ttm_0t)
summary(ttm_1t)
summary(ttm_2t)
summary(ttm_3t)
summary(ttm_4t)

nrow(ttm_0t)
nrow(ttm_1t)
nrow(ttm_2t)
nrow(ttm_3t)
nrow(ttm_4t)

# 1. Compute transfers for ttm
ttm_transfers = ttm_4t |> rename(tt_4t = travel_time_p50) |>
  left_join(ttm_3t |> rename(tt_3t = travel_time_p50), by = c("from_id", "to_id")) |>
  left_join(ttm_2t |> rename(tt_2t = travel_time_p50), by = c("from_id", "to_id")) |>
  left_join(ttm_1t |> rename(tt_1t = travel_time_p50), by = c("from_id", "to_id")) |>
  left_join(ttm_0t |> rename(tt_0t = travel_time_p50), by = c("from_id", "to_id")) |>
  mutate(
    transfers_required = case_when(
      !is.na(tt_0t) ~ 0,
      !is.na(tt_1t) ~ 1,
      !is.na(tt_2t) ~ 2,
      !is.na(tt_3t) ~ 3,
      !is.na(tt_4t) ~ 4,
      TRUE ~ NA_real_
    )
  )

summary(ttm_transfers)
table(ttm_transfers$transfers_required)

# 2. Add transfers to jittering data
# Load jittering_grid from section I.
jittering_grid_transfers = jittering_grid |>
  left_join(ttm_transfers |> select(from_id, to_id, transfers_required), by = c("id_grid_origin" = "from_id", "id_grid_destination" = "to_id")) |> 
  mutate(na = ifelse(is.na(transfers_required), 1, 0))

nrow(jittering_grid_transfers) # 18997
nrow(jittering_grid_transfers |> filter(is.na(transfers_required))) # 2649
sum(jittering_grid_transfers$na) # 2649
nrow(jittering_grid_transfers |> filter(is.na(transfers_required))) / nrow(jittering_grid_transfers) # 13.9%

# Aggregate by grid, parish and municipality
aggregated_commuting_for_transfers = function(grid) {
  return (
    grid |> 
      summarise(
        # Count number of jitters aggregated
        nr_jitters = n(),
        # Count nr of NAs
        nr_jitters_na = sum(na),
        # Transfers average, weighted by trips number
        weighted_mean_transfers = round(weighted.mean(transfers_required, trips, na.rm=TRUE), digits=2),
        # Total trips 
        trips = sum(trips),
        # Total transfers
        total_transfers = sum(transfers_required, na.rm = TRUE)
      ) |> 
      ungroup() 
  )
}

grid_transfers = aggregated_commuting_for_transfers(jittering_grid_transfers |> group_by(id_grid_origin))
summary(grid_transfers)

grid_transfers_sf = grid |> select(id, geom) |> left_join(grid_transfers, by=c("id" = "id_grid_origin"))
# mapview(grid_transfers_sf, zcol = "weighted_mean_transfers")

freguesia_transfers = aggregated_commuting_for_transfers(jittering_grid_transfers |> group_by(Origin_dicofre24))

freguesia_transfers_sf = freguesias |> select(dtmnfr, geom) |> left_join(freguesia_transfers, by=c("dtmnfr" = "Origin_dicofre24"))
# mapview(freguesia_transfers_sf, zcol = "weighted_mean_transfers")

municipio_transfers = aggregated_commuting_for_transfers(
  jittering_grid_transfers |>
    left_join(freguesias |> select(dtmnfr, municipio), by=c("Origin_dicofre24" = "dtmnfr")) |>
    group_by(municipio) 
)
municipio_transfers_sf = municipios |> select(municipio, geom) |> left_join(municipio_transfers, by=c("municipio" = "municipio"))
# mapview(municipio_transfers_sf, zcol = "weighted_mean_transfers")


st_write(grid_transfers_sf, IMPT_URL(sprintf("%s/grid_transfers.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(grid_transfers, IMPT_URL(sprintf("%s/grid_transfers.csv", output_dir)), row.names = FALSE) 
st_write(freguesia_transfers_sf, IMPT_URL(sprintf("%s/freguesia_transfers.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(freguesia_transfers, IMPT_URL(sprintf("%s/freguesia_transfers.csv", output_dir)), row.names = FALSE) 
st_write(municipio_transfers_sf, IMPT_URL(sprintf("%s/municipio_transfers.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(municipio_transfers, IMPT_URL(sprintf("%s/municipio_transfers.csv", output_dir)), row.names = FALSE)





# Accessibility Gap -------------------------------------------------------

# OD pairs with trip weights from the jittered commuting flows.
# We need both origin, destination AND trips for the weighted mean in process_ttm.
od_pairs <- read_csv(IMPT_URL("/mobility_commuting/jittering_grid.csv")) |>
  select(id_grid_origin, id_grid_destination, trips) |>
  mutate(
    id_grid_origin      = as.integer(id_grid_origin),
    id_grid_destination = as.integer(id_grid_destination)
  )

# Destination grid cells used in actual commuting flows (job locations).
# FIX: was incorrectly using origin IDs before.
poi_h_dest <- unique(od_pairs$id_grid_destination[!is.na(od_pairs$id_grid_destination)])

# process_ttm: for each origin grid cell, compute the trip-weighted mean
# travel time to all reachable destination cells (actual job locations).
# FIX 1: od_pairs now passed in and joined so weighted.mean has real trip weights.
# FIX 2: uses 120-min TTM — avoids artificial 60-min cap collapsing the gap.
# FIX 3: NAs (truly unreachable within 120 min) are left as NA, not imputed.
process_ttm <- function(path, od_pairs, dests, tag) {
  if (!file.exists(path)) {
    message(sprintf("File not found, skipping: %s", path))
    return(data.frame(hex_id = character()))
  }
  readRDS(path) |>
    mutate(from_id = as.integer(from_id), to_id = as.integer(to_id)) |>
    filter(to_id %in% dests) |>
    # Join trip weights: only keep OD pairs that exist in the jittered flows
    inner_join(od_pairs, by = c("from_id" = "id_grid_origin", "to_id" = "id_grid_destination")) |>
    group_by(from_id) |>
    # Weighted mean travel time across all destinations, weighted by nr of trips
    summarise(
      !!tag        := weighted.mean(travel_time_p50, trips, na.rm = TRUE),
      total_trips  := sum(trips, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(hex_id = from_id) |>
    mutate(hex_id = as.character(hex_id))
}

res_car  <- process_ttm(
  IMPT_URL("/ttm/ttm_h3_res8/ttm_car_120min_202602040800.rds"),
  od_pairs, poi_h_dest, "time_car"
)
res_peak <- process_ttm(
  IMPT_URL("/ttm/ttm_h3_res8/ttm_transit_120min_202602040800_1transfers.rds"),
  od_pairs, poi_h_dest, "time_pt_peak"
)

summary(res_car)
summary(res_peak)
nrow(res_car)   # origin cells with at least one reachable job by car
nrow(res_peak)  # same for transit — expect fewer

# Total trips per origin grid cell (for weighting at higher scales)
jobs_grid_redux <- od_pairs |>
  group_by(hex_id = as.character(id_grid_origin)) |>
  summarise(trips = sum(trips, na.rm = TRUE), .groups = "drop")

# Build grid-level gap table -----------------------------------------------
# FIX: removed grid_freg_mun (undefined here); build from grid + spatial join.
# FIX: only filter where car time is missing; transit NAs are kept as they
#      represent the largest penalty (unreachable by PT within 120 min).
# accessibility_gap: for each origin hex cell, the difference in minutes between
# the trip-weighted average PT travel time and the trip-weighted average car travel
# time, across all actual commuting destinations (OD pairs from jittered flows).
# Positive value = PT takes longer than car by that many minutes.
# trips = total commuting trips originating from this hex cell (all destinations).
hex_stats_gap <- grid |>
  st_drop_geometry() |>
  select(grid_id = id) |>
  mutate(grid_id = as.character(grid_id)) |>
  left_join(res_car  |> select(hex_id, time_car),   by = c("grid_id" = "hex_id")) |>
  left_join(res_peak |> select(hex_id, time_pt_peak), by = c("grid_id" = "hex_id")) |>
  left_join(jobs_grid_redux, by = c("grid_id" = "hex_id")) |>
  # Only drop cells where car time is unknown (no matched commuting flows)
  filter(!is.na(time_car) & time_car > 0) |>
  mutate(
    # Where transit cannot reach destination within 120 min, assign 120 as penalty.
    # This avoids hiding the true gap for cells unserved by PT.
    time_pt_peak_for_gap = if_else(is.na(time_pt_peak), 120, time_pt_peak),
    # Gap in minutes (at origin hex level): positive = PT slower than car
    accessibility_gap = time_pt_peak_for_gap - time_car
  )

# Attach freg and municipio ids via spatial join
hex_stats_gap <- hex_stats_gap |>
  left_join(
    st_join(
      st_centroid(grid),
      freguesias |> select(freg_id = dtmnfr, mun_id = municipio),
      join = st_within
    ) |>
      st_drop_geometry() |>
      mutate(grid_id = as.character(id)) |>
      select(grid_id, freg_id, mun_id),
    by = "grid_id"
  )

summary(hex_stats_gap)
# mapview(grid |> left_join(hex_stats_gap, by = c("id" = "grid_id")), zcol = "accessibility_gap")
# mapview(grid |> left_join(hex_stats_gap, by = c("id" = "grid_id")), zcol = "time_car")
# mapview(grid |> left_join(hex_stats_gap, by = c("id" = "grid_id")), zcol = "time_pt_peak")

# at freguesia level: weighted mean by trips --------------------------------
freg_stats_gap <- hex_stats_gap |>
  filter(!is.na(freg_id)) |>
  group_by(freg_id) |>
  summarise(
    time_car           = weighted.mean(time_car,             trips, na.rm = TRUE),
    time_pt_peak       = weighted.mean(time_pt_peak_for_gap, trips, na.rm = TRUE),
    total_trips        = sum(trips, na.rm = TRUE),
    n_cells_no_transit = sum(is.na(time_pt_peak)),   # cells where PT unreachable
    .groups = "drop"
  ) |>
  mutate(accessibility_gap = time_pt_peak - time_car) |>
  filter(!is.nan(time_car))

freg_stats_gap_sf <- freguesias |> select(dtmnfr, geom) |>
  left_join(freg_stats_gap, by = c("dtmnfr" = "freg_id"))

# mapview(freg_stats_gap_sf, zcol = "accessibility_gap")
# mapview(freg_stats_gap_sf, zcol = "time_car")
# mapview(freg_stats_gap_sf, zcol = "time_pt_peak")

# at municipio level: weighted mean by trips --------------------------------
mun_stats_gap <- hex_stats_gap |>
  filter(!is.na(mun_id)) |>
  group_by(mun_id) |>
  summarise(
    time_car           = weighted.mean(time_car,             trips, na.rm = TRUE),
    time_pt_peak       = weighted.mean(time_pt_peak_for_gap, trips, na.rm = TRUE),
    total_trips        = sum(trips, na.rm = TRUE),
    n_cells_no_transit = sum(is.na(time_pt_peak)),
    .groups = "drop"
  ) |>
  mutate(accessibility_gap = time_pt_peak - time_car) |>
  filter(!is.nan(time_car))

mun_stats_gap_sf <- municipios |> select(municipio, geom) |>
  left_join(mun_stats_gap, by = c("municipio" = "mun_id"))

# mapview(mun_stats_gap_sf, zcol = "accessibility_gap")
# mapview(mun_stats_gap_sf, zcol = "time_car")
# mapview(mun_stats_gap_sf, zcol = "time_pt_peak")

# Export ------------------------------------------------------------------
write.csv(hex_stats_gap,  IMPT_URL(sprintf("%s/hex_stats_gap.csv",  output_dir)), row.names = FALSE)
write.csv(freg_stats_gap, IMPT_URL(sprintf("%s/freg_stats_gap.csv", output_dir)), row.names = FALSE)
write.csv(mun_stats_gap,  IMPT_URL(sprintf("%s/mun_stats_gap.csv",  output_dir)), row.names = FALSE)

st_write(freg_stats_gap_sf, IMPT_URL(sprintf("%s/freg_stats_gap.gpkg", output_dir)), delete_dsn = TRUE)
st_write(mun_stats_gap_sf,  IMPT_URL(sprintf("%s/mun_stats_gap.gpkg",  output_dir)), delete_dsn = TRUE)

