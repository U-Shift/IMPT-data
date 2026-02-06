

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

POINTS = pois_healthcare ## ADD FILTER BY type
ttm = readRDS_remote(IMPT_URL("/ttm/ttm_h3_res8/ttm_car_60min_202602040800.rds"))

grid_points <- st_join(grid, POINTS |> rename(id_point = id), join = st_intersects) |>
  group_by(id) |>
  summarise(healthcare = n()) |> ## MUDAR PARA hospitals and CS, com um group_by(type) ?
  # id as character
  mutate(id = as.character(id))
# mapview(grid_points, zcol = "health")

access_health_15 <- accessibility::cumulative_cutoff(
  travel_matrix = ttm, 
  land_use_data = grid_points,
  opportunity = 'healthcare',
  travel_cost = 'travel_time_p50',
  cutoff = 15
)
access_health_30 <- accessibility::cumulative_cutoff(
  travel_matrix = ttm, 
  land_use_data = grid_points,
  opportunity = 'healthcare',
  travel_cost = 'travel_time_p50',
  cutoff = 30 
)


# Nearest opportunity -----------------------------------------------------

cost_to_closest = accessibility::cost_to_closest(
  travel_matrix = ttm,
  land_use_data = grid_points,
  opportunity = 'healthcare',
  travel_cost = 'travel_time_p50'
)

# Combine results ---------------------------------------------------------

grid_accessibility = grid_population |>
  mutate(id = as.character(id)) |>
  left_join(data.frame(access_health_15) |> select(id, access_health_15 = healthcare), by = "id") |>
  left_join(data.frame(access_health_30) |> select(id, access_health_30 = healthcare), by = "id") |>
  left_join(data.frame(cost_to_closest) |> select(id, cost_to_closest_health = travel_time_p50), by = "id")

mapview(grid_accessibility, zcol = "access_health_15")
mapview(grid_accessibility, zcol = "access_health_30")
mapview(grid_accessibility, zcol = "cost_to_closest_health")


# Aggregate by freguesia and weight by population  ------------------------------------------------------------

freguesia_accessibility = freguesias |> 
  st_transform(st_crs(grid_accessibility)) |>
  st_join(grid_accessibility, join = st_intersects) |>
  st_drop_geometry() |>
  group_by(dtmnfr) |>
  summarise(
    # Weighted means by population
    access_health_15 = weighted.mean(access_health_15, residents, na.rm=TRUE),
    access_health_30 = weighted.mean(access_health_30, residents, na.rm=TRUE),
    cost_to_closest_health = weighted.mean(cost_to_closest_health, residents, na.rm=TRUE),
    # Ratio of population with access
    access_health_15_ratio = sum(ifelse(is.na(access_health_15), 0, residents)) / sum(residents),
    access_health_30_ratio = sum(ifelse(is.na(access_health_30), 0, residents)) / sum(residents),
    # Weighted means * ratio
    access_health_15_weighted = access_health_15 * access_health_15_ratio,
    access_health_30_weighted = access_health_30 * access_health_30_ratio,
    # Demographics
    buildings = sum(buildings),
    families = sum(families),
    kids = sum(kids),
    elder = sum(elder),
    residents = sum(residents),
  ) |>
  # Get geometry back
  left_join(freguesias |> select(dtmnfr, geom), by = "dtmnfr") |>
  st_as_sf()

mapview(freguesia_accessibility, zcol = "access_health_15")
mapview(freguesia_accessibility, zcol = "access_health_30")
mapview(freguesia_accessibility, zcol = "cost_to_closest_health")
mapview(freguesia_accessibility, zcol = "access_health_15_weighted")
mapview(freguesia_accessibility, zcol = "access_health_30_weighted")

# Save results ------------------------------------------------------------
write.csv(freguesia_accessibility |> st_drop_geometry(), "data/accessibility_freguesias_healthcare.csv", row.names = FALSE)


