# Access to opportunities
# Purpose   Determine number of opportunities reachable within threshold (travel time)
# Scale     hex, parish, municipality
# Issue     https://github.com/U-Shift/IMPT-data/issues/7


library(mapview)

output_dir <- "/accessibility/r8"

# Travel time matrix ---------------------------------------------------------------

# ttm_bike_60 = impt_read("/ttm/ttm_h3_res9/ttm_bicycle_60min_202602040800.rds")
# nrow(ttm_bike_60 |> filter(travel_time_p50<=55))
# ttm_bike_120 = impt_read("/ttm/ttm_h3_res9/ttm_bicycle_120min_202602040800.rds")
# nrow(ttm_bike_120)
# nrow(ttm_bike_120 |> filter(travel_time_p50<=55))
# View(ttm_bike_120)
ttm_root <- "/ttm/ttm_h3_res8"
ttm_timings <- list(5, 10, 15, 30, 45, 60, 75, 90)
ttm_list <- list(
  # Mode, Travel time matrix file, Time cutoffs to consider
  list("walk", "ttm_walk_120min_202602040800.rds", ttm_timings),
  list("bike", "ttm_bicycle_120min_202602040800.rds", ttm_timings),
  list("car", "ttm_car_120min_202602040800.rds", ttm_timings),
  list("transit_1t", "ttm_transit_120min_202602040800_1transfers.rds", ttm_timings),
  list("transit_2t", "ttm_transit_120min_202602040800_2transfers.rds", ttm_timings)
)


# POIs ---------------------------------------------------------------
# From OSm
pois_health <- impt_read("/pois/healthcare.gpkg") |> mutate(n = 1)
pois_supermarket <- impt_read("/pois/supermarket.gpkg") |> mutate(n = 1)
pois_green <- impt_read("/pois/green.gpkg") |> mutate(n = 1)
pois_recreation <- impt_read("/pois/recreation.gpkg") |> mutate(n = 1)
pois_schools <- impt_read("/pois/schools.gpkg") |> mutate(n = 1)
# pois_jobs_roads = impt_read("/pois/pois_jobs_imob_jt50.gpkg") |> rename(n=trips)
pois_jobs_buildings <- impt_read("/pois/pois_jobs_imob_jt50_buildings.gpkg") |> rename(n = trips)
pois_jobs <- pois_jobs_buildings
pois_transit <- impt_read("/pois/transit_stops.gpkg") |> mutate(n = 1)

pois_bus <- pois_transit |> filter(agency %in% c("Carris", "Cascais Próxima", "Transportes Colectivos do Barreiro", "Viação Alvorada"))
pois_mass <- pois_transit |> filter(agency %in% c("CP - Comboios de Portugal", "Fertagus", "Metro Transportes do Sul", "Metropolitano de Lisboa, E.P.E.", "TTSL - Transtejo Soflusa"))
# mapview(pois_bus, zcol="agency")
# mapview(pois_mass, zcol="agency")

pois_list <- list(
  # POI type, sf data.frame with points
  list("health", pois_health),
  list("health_primary", pois_health |> filter(type == "Centro de Saúde")),
  list("health_hospital", pois_health |> filter(type == "Hospital")),
  list("groceries", pois_supermarket),
  list("greenspaces", pois_green),
  list("recreation", pois_recreation),
  list("schools", pois_schools),
  list("schools_primary", pois_schools |> filter(grepl("Cycle", type))),
  list("schools_high", pois_schools |> filter(grepl("High", type))),
  list("jobs", pois_jobs),
  list("transit", pois_transit |> mutate(n = 1)),
  list("transit_bus", pois_bus |> mutate(n = 1)),
  list("transit_mass", pois_mass |> mutate(n = 1))
)


# Grid population ---------------------------------------------------------------
grid_census <- census |> st_join(grid, join = st_within)

grid_population <- grid_census |>
  st_drop_geometry() |> # drop geometry for counting
  group_by(id.y) |>
  summarise(
    buildings = sum(N_EDIFICIOS_CLASSICOS),
    families = sum(N_NUCLEOS_FAMILIARES),
    residents = sum(N_INDIVIDUOS),
    kids = sum(N_INDIVIDUOS_0_14), # 0-14
    young = sum(N_INDIVIDUOS_15_24), # 15-24
    active = sum(N_INDIVIDUOS_25_64), # 25-64
    elder = sum(N_INDIVIDUOS_65_OU_MAIS), # 65+
    male = sum(N_INDIVIDUOS_H),
    female = sum(N_INDIVIDUOS_M)
  ) |>
  rename(id = id.y) |>
  filter(!is.na(id)) |>
  ungroup()

grid_population <- grid |>
  left_join(grid_population, by = "id") |>
  mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0)))

# mapview(grid_population, zcol = "residents")
# mapview(grid_population, zcol = "elder")
# mapview(grid_population, zcol = "buildings")

grid_points_population <- st_centroid(grid_population)

# Calculations: Number of opportunities -------------------------------------------------

# 1. Count opportunities accessible
for (i in 1:length(pois_list)) {
  pois_name <- pois_list[[i]][[1]]
  pois_data <- pois_list[[i]][[2]]
  if ("id" %in% colnames(pois_data)) {
    pois_data <- pois_data |> select(-id)
  }
  message(paste("Processing POI category:", pois_name))

  grid_points <- st_join(grid, pois_data, join = st_intersects) |>
    group_by(id) |>
    # Sum the dummy column. 'na.rm = TRUE' treats the NAs (non-matches) as 0.
    summarise(!!pois_name := sum(n, na.rm = TRUE)) |> # mapview(grid_points, zcol=pois_name)
    st_drop_geometry()


  for (z in 1:length(ttm_list)) {
    ttm_name <- ttm_list[[z]][[1]]
    ttm_data <- impt_read(sprintf("%s/%s", ttm_root, ttm_list[[z]][[2]])) |>
      mutate(
        from_id = as.integer(from_id),
        to_id = as.integer(to_id)
      )
    ttm_cutoffs <- ttm_list[[z]][[3]]
    message(paste("> Processing travel time matrix:", ttm_name))

    for (cutoff in ttm_cutoffs) {
      message(paste(">> Processing cutoff:", cutoff, "minutes"))
      colname <- paste0("access_", pois_name, "_", ttm_name, "_", cutoff, "min")
      access_result <- accessibility::cumulative_cutoff(
        travel_matrix = ttm_data,
        land_use_data = grid_points,
        opportunity = pois_name,
        travel_cost = "travel_time_p50",
        cutoff = cutoff
      ) |> rename(
        !!colname := sym(pois_name)
      )

      ncolname <- paste("n_", pois_name, sep = "")
      grid_points_access <- grid_points |>
        left_join(access_result, by = "id") |>
        rename(!!ncolname := sym(pois_name))

      # mapview(grid |> left_join(grid_points_access), zcol = colname)
      write.csv(grid_points_access, IMPT_URL(sprintf("%s/%s.csv", output_dir, colname)), row.names = FALSE)
      message(paste(">>> Saved result for", colname, ", procedding with next..."))
    }
  }
}

# 2. Aggregate by parish, population weighted
accessibility_modes <- list(
  list("walk", ttm_timings),
  list("bike", ttm_timings),
  list("car", ttm_timings),
  list("transit_1t", ttm_timings),
  list("transit_2t", ttm_timings)
)

accessibility_measures <- list(
  # POI type, Population relates to, [ Mode, Time ]
  list("health", "residents", accessibility_modes),
  list("health_primary", "residents", accessibility_modes),
  list("health_hospital", "residents", accessibility_modes),
  list("health", "elder", accessibility_modes),
  list("health_primary", "elder", accessibility_modes),
  list("health_hospital", "elder", accessibility_modes),
  list("schools_primary", "kids", accessibility_modes),
  list("schools_high", "young", accessibility_modes),
  list("greenspaces", "residents", accessibility_modes),
  list("recreation", "residents", accessibility_modes),
  list("groceries", "residents", accessibility_modes),
  list("jobs", "residents", accessibility_modes),
  list("jobs", "active", accessibility_modes),
  list("transit", "residents", accessibility_modes),
  list("transit_bus", "residents", accessibility_modes),
  list("transit_mass", "residents", accessibility_modes)
)


grid_access <- grid_population
freguesia_accessibility <- freguesias |>
  st_transform(st_crs(grid_access)) |>
  st_join(grid_access, join = st_intersects) |>
  st_drop_geometry() |>
  group_by(dtmnfr) |>
  summarise(
    # Demographics
    buildings = sum(buildings),
    families = sum(families),
    kids = sum(kids),
    elder = sum(elder),
    residents = sum(residents)
  ) |>
  # Get geometry back
  left_join(freguesias |> select(dtmnfr, geom), by = "dtmnfr") |>
  st_as_sf()

municipio_accessibility <- municipios |>
  st_transform(st_crs(grid_access)) |>
  st_join(grid_access, join = st_intersects) |>
  st_drop_geometry() |>
  group_by(municipio) |>
  summarise(
    # Demographics
    buildings = sum(buildings),
    families = sum(families),
    kids = sum(kids),
    elder = sum(elder),
    residents = sum(residents)
  ) |>
  # Get geometry back
  left_join(municipios |> select(municipio, geom), by = "municipio") |>
  st_as_sf()

# mapview(freguesia_accessibility, zcol = "residents")
# mapview(municipio_accessibility, zcol = "residents")

for (i in 1:length(accessibility_measures)) {
  poi_name <- accessibility_measures[[i]][[1]]
  census_col <- accessibility_measures[[i]][[2]]
  modes_times <- accessibility_measures[[i]][[3]]
  message(paste("Processing accessibility for POI category", poi_name, "census", census_col))

  for (m in 1:length(modes_times)) {
    mode_name <- modes_times[[m]][[1]]
    mode_times <- modes_times[[m]][[2]]
    message(paste("> Processing mode", mode_name))

    for (mt in 1:length(mode_times)) {
      mode_time <- mode_times[[mt]]
      message(paste(">> Processing time", mode_time, "minutes"))
      colname <- paste0("access_", poi_name, "_", mode_name, "_", mode_time, "min")
      ncolname <- paste("n_", poi_name, sep = "")
      access <- impt_read(sprintf("%s/%s.csv", output_dir, colname))
      if (m > 1 | mt > 1) {
        access <- access |> select(-sym(ncolname))
      }

      if (!(colname %in% names(grid_access))) { # to avoid error when same pois for different populations
        grid_access <- grid_access |>
          left_join(access, by = "id")
      }

      freguesia_access <- freguesia_accessibility |>
        select(dtmnfr, geom) |>
        st_join(grid_access, join = st_intersects) |>
        st_drop_geometry() |>
        group_by(dtmnfr)

      municipio_access <- municipio_accessibility |>
        select(municipio, geom) |>
        st_join(grid_access, join = st_intersects) |>
        st_drop_geometry() |>
        group_by(municipio)

      colname_population <- paste(colname, "_", census_col, sep = "")

      freguesia_access <- freguesia_access |>
        summarise(
          # Weighted means by population
          !!colname_population := weighted.mean(get(colname), get(census_col), na.rm = TRUE),
          # N sum
          !!ncolname := sum(get(ncolname), na.rm = TRUE)
        )
      municipio_access <- municipio_access |> summarise(
        # Weighted means by population
        !!colname_population := weighted.mean(get(colname), get(census_col), na.rm = TRUE),
        # N sum
        !!ncolname := sum(get(ncolname), na.rm = TRUE)
      )

      freguesia_access <- freguesia_access |> mutate(!!colname_population := round(get(colname_population), digits = 2))
      municipio_access <- municipio_access |> mutate(!!colname_population := round(get(colname_population), digits = 2))
      freguesia_access <- freguesia_access |> mutate(!!ncolname := round(get(ncolname), digits = 2))
      municipio_access <- municipio_access |> mutate(!!ncolname := round(get(ncolname), digits = 2))

      if (ncolname %in% names(freguesia_accessibility)) {
        freguesia_access <- freguesia_access |> select(-sym(ncolname))
      }
      if (ncolname %in% names(municipio_accessibility)) {
        municipio_access <- municipio_access |> select(-sym(ncolname))
      }

      freguesia_accessibility <- freguesia_accessibility |>
        left_join(freguesia_access, by = "dtmnfr")

      municipio_accessibility <- municipio_accessibility |>
        left_join(municipio_access, by = "municipio")
      # View(freguesia_accessibility|>st_drop_geometry())
    }
  }
}

st_write(freguesia_accessibility, IMPT_URL(sprintf("%s/accessibility_freguesia.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(freguesia_accessibility |> st_drop_geometry(), IMPT_URL(sprintf("%s/accessibility_freguesia.csv", output_dir)), row.names = FALSE)
st_write(municipio_accessibility, IMPT_URL(sprintf("%s/accessibility_municipio.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(municipio_accessibility |> st_drop_geometry(), IMPT_URL(sprintf("%s/accessibility_municipio.csv", output_dir)), row.names = FALSE)
st_write(grid_access, IMPT_URL(sprintf("%s/accessibility_grid.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(grid_access |> st_drop_geometry(), IMPT_URL(sprintf("%s/accessibility_grid.csv", output_dir)), row.names = FALSE)
#
# mapview(municipio_accessibility, zcol = "access_transit_bus_transit_1t_5min_residents")
# mapview(freguesia_accessibility, zcol = "access_transit_transit_1t_5min_residents")
# mapview(freguesia_accessibility, zcol = "access_transit_mass_transit_1t_5min_residents")
# mapview(freguesia_accessibility, zcol = "access_transit_mass_transit_1t_15min_residents")

# mapview(freguesia_accessibility, zcol = "access_jobs_walk_45min") +
#   mapview(freguesia_accessibility, zcol = "access_jobs_bike_45min") +
#   mapview(freguesia_accessibility, zcol = "access_jobs_car_45min") +
#   mapview(freguesia_accessibility, zcol = "access_jobs_transit_2t_45min")
# #
# mapview(grid_access, zcol = "access_jobs_walk_45min") +
#   mapview(grid_access, zcol = "access_jobs_bike_45min") +
#   mapview(grid_access, zcol = "access_jobs_car_45min") +
#   mapview(grid_access, zcol = "access_jobs_transit_2t_45min")
# #   mapview(pois_health)
