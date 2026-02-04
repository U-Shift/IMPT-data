

# Grid info ---------------------------------------------------------------
grid_census = census |> st_join(grid, join = st_within)
  
grid_population = grid_census |>
  st_drop_geometry() |>   # drop geometry for counting
  group_by(id.y) |>
  summarise(buildings = sum(N_EDIFICIOS_CLASSICOS),
            families = sum(N_NUCLEOS_FAMILIARES),
            residents = sum(N_INDIVIDUOS),
            kids = sum(N_INDIVIDUOS_0_14), # 0-14
            elder = sum(N_INDIVIDUOS_65_OU_MAIS), # 65+
            male = sum(N_INDIVIDUOS_H),
            female = sum(N_INDIVIDUOS_M)
            ) |> 
  rename(id = id.y) |> 
  filter(!is.na(id)) |>
  ungroup()

grid_population = grid |>
  left_join(grid_population, by = "id") |>
  mutate(across(where(is.numeric), ~tidyr::replace_na(.x, 0)))

# mapview(grid_population, zcol = "residents")
# mapview(grid_population, zcol = "elder")
# mapview(grid_population, zcol = "buildings")

grid_points_population = st_centroid(grid_population)


# Number of opportunities -------------------------------------------------


departure_datetime_HP = as.POSIXct("04-02-2026 8:00:00", format = "%d-%m-%Y %H:%M:%S") # quarta-feira
mode_egress = "WALK" # can be BIKE
max_walk_time = 20 # in minutes
max_trip_duration = 90 # in minutes
time_window = 120 # in minutes
time_intervals <- seq(0, 100, 10) # 10-minute intervals
time_cutoff = 60 # 60 minutes for all modes, we cut off then

# Health
# calculate travel time matrix
ttm_health_CAR <- r5r::travel_time_matrix(
  r5r_network = r5r_core,
  origins = grid_points_population,
  destinations = pois_healthcare,
  mode = "CAR",
  departure_datetime = departure_datetime_HP,
  max_walk_time = max_walk_time,
  time_window = time_window,
  progress = FALSE
)

access_health_15 <- accessibility::cumulative_cutoff(
  travel_matrix = ttm_PT, 
  land_use_data = POINTS,
  opportunity = 'healthcare',
  travel_cost = 'travel_time_p50',
  cutoff = 15
)
access_health_30 <- accessibility::cumulative_cutoff(
  travel_matrix = ttm_PT, 
  land_use_data = POINTS,
  opportunity = 'healthcare',
  travel_cost = 'travel_time_p50',
  cutoff = 30 
)


# Nearest opportunity -----------------------------------------------------

# Healt
hospitals = pois_healthcare |> filter(type=="Hospital")
grid_nearest_health = st_nearest_feature(
  grid_points_population, 
  hospitals
)
grid_points_nearest_hospital = data.frame(hospitals$id[grid_nearest_health])
colnames(grid_points_nearest_hospital) = c("id")
grid_points_nearest_hospital = grid_points_nearest_hospital |> 
  left_join(pois_healthcare |>  select(id), by = "id") |>
  st_as_sf()
mapview(grid_points_nearest_hospital)

ttm_r_hospital_CAR = travel_time_matrix(
  r5r_network = r5r_core,
  origins = grid_points_population,
  destinations = grid_points_nearest_hospital,
  mode = "CAR", 
  mode_egress = "WALK",
  departure_datetime = departure_datetime_HP,
  max_walk_time = max_walk_time,
  max_trip_duration = 60,
  verbose = FALSE
)

