# Accessibility Gap - Time -------------------------------------------------------
# grid_commuting.csv (produced in Section I above) already contains:
#   id_grid_origin, avg_tt_car, avg_tt_transit_1t_120m_15w, trips
#   where avg_tt_* = trip-weighted mean travel time per origin grid cell.
# This is exactly the equivalent of grid_commuting_money_*.csv for time.
# No need to re-read raw TTM files or re-compute weighted means.

tt_grid <- read.csv(IMPT_URL("/mobility_commuting/grid_commuting.csv")) |>
    mutate(id_grid_origin = as.character(id_grid_origin))

# Grid-level time gap -----------------------------------------------
# avg_tt_car and avg_tt_transit_1t_120m_15w are already trip-weighted means.
# Transit NAs (unreachable within 120 min) → assign 120 min as penalty.
hex_stats_gap <- tt_grid |>
  select(
    grid_id = id_grid_origin, 
    time_car = avg_tt_car,
    time_pt_peak = avg_tt_transit_1t_120m_15w, 
    trips
  ) |>
  filter(!is.na(time_car) & time_car > 0) |>
  mutate(
    time_pt_peak_for_gap = if_else(is.na(time_pt_peak), 120, time_pt_peak),
    # Gap in minutes: positive = PT slower than car
    accessibility_gap = round(time_pt_peak_for_gap - time_car, 2),
    
    # Relative index: 0 is parity, positive = PT xtimes slower than car, negative = PT x times faster than car
    relative_gap_time = ifelse(time_pt_peak_for_gap >= time_car,
                                (time_pt_peak_for_gap / time_car) - 1,
                                -((time_car / time_pt_peak_for_gap) - 1)),
    relative_gap_time = round(relative_gap_time, 2)
  ) |>
  left_join(grid_freg_mun |> select(grid_id, freg_id, mun_id), by = "grid_id")

summary(hex_stats_gap)
# mapview(grid |> mutate(grid_id = as.character(id)) |> left_join(hex_stats_gap), zcol = "accessibility_gap")
mapview(grid |> mutate(grid_id = as.character(id)) |> left_join(hex_stats_gap), zcol = "relative_gap_time")
# mapview(grid |> mutate(grid_id = as.character(id)) |> left_join(hex_stats_gap), zcol = "time_car")
# mapview(grid |> mutate(grid_id = as.character(id)) |> left_join(hex_stats_gap), zcol = "time_pt_peak")

# at freguesia level: re-weight grid means by trips --------------------------------
freg_stats_gap <- hex_stats_gap |>
    filter(!is.na(freg_id)) |>
    group_by(freg_id) |>
    summarise(
        time_car = round(weighted.mean(time_car, trips, na.rm = TRUE),2),
        time_pt_peak = round(weighted.mean(time_pt_peak_for_gap, trips, na.rm = TRUE),2),
        total_trips = sum(trips, na.rm = TRUE),
        n_cells_no_transit = sum(is.na(time_pt_peak)),
        .groups = "drop"
    ) |>
  mutate(
    freg_id   = as.character(freg_id),
    time_pt_peak_for_gap = if_else(is.na(time_pt_peak), 120, time_pt_peak), # this is irrelevant at this level
    # Gap in minutes: positive = PT slower than car
    accessibility_gap = round(time_pt_peak_for_gap - time_car, 2),
    
    # Relative index: 0 is parity, positive = PT xtimes slower than car, negative = PT x times faster than car
    relative_gap_time = ifelse(time_pt_peak_for_gap >= time_car,
                                (time_pt_peak_for_gap / time_car) - 1,
                                -((time_car / time_pt_peak_for_gap) - 1)),
    relative_gap_time = round(relative_gap_time, 2)
  ) |>
    filter(!is.nan(time_car))

freg_stats_gap_sf <- freguesias |>
    select(dtmnfr, geom) |>
    left_join(freg_stats_gap, by = c("dtmnfr" = "freg_id"))

mapview(freg_stats_gap_sf, zcol = "accessibility_gap")
mapview(freg_stats_gap_sf, zcol = "relative_gap_time")
# mapview(freg_stats_gap_sf, zcol = "time_car")
# mapview(freg_stats_gap_sf, zcol = "time_pt_peak")

# at municipio level: re-weight grid means by trips --------------------------------
municipios_id <- read.csv("useful_data/municipios_id.csv") |> mutate(mun_id = as.character(mun_id))

mun_stats_gap <- hex_stats_gap |>
    filter(!is.na(mun_id)) |>
    group_by(mun_id) |>
    summarise(
        time_car = round(weighted.mean(time_car, trips, na.rm = TRUE),2),
        time_pt_peak = round(weighted.mean(time_pt_peak_for_gap, trips, na.rm = TRUE),2),
        total_trips = sum(trips, na.rm = TRUE),
        n_cells_no_transit = sum(is.na(time_pt_peak)),
        .groups = "drop"
    ) |>
  mutate(
    mun_id   = as.character(mun_id),
    time_pt_peak_for_gap = if_else(is.na(time_pt_peak), 120, time_pt_peak),
    # Gap in minutes: positive = PT slower than car
    accessibility_gap = round(time_pt_peak_for_gap - time_car, 2),
    
    # Relative index: 0 is parity, positive = PT xtimes slower than car, negative = PT x times faster than car
    relative_gap_time = ifelse(time_pt_peak_for_gap >= time_car,
                               (time_pt_peak_for_gap / time_car) - 1,
                               -((time_car / time_pt_peak_for_gap) - 1)),
    relative_gap_time = round(relative_gap_time, 2)
  ) |>
    filter(!is.nan(time_car))

mun_stats_gap_sf <- municipios |>
    select(municipio, geom) |>
    left_join(municipios_id) |>
    left_join(mun_stats_gap, by = "mun_id")

mapview(mun_stats_gap_sf, zcol = "accessibility_gap")
mapview(mun_stats_gap_sf, zcol = "relative_gap_time")
# mapview(mun_stats_gap_sf, zcol = "time_car")
# mapview(mun_stats_gap_sf, zcol = "time_pt_peak")

# Export ------------------------------------------------------------------
output_dir <- "/mobility_commuting"
write.csv(hex_stats_gap, IMPT_URL(sprintf("%s/grid_accessibility_gap_time.csv", output_dir)), row.names = FALSE)
write.csv(freg_stats_gap, IMPT_URL(sprintf("%s/freg_accessibility_gap_time.csv", output_dir)), row.names = FALSE)
write.csv(mun_stats_gap, IMPT_URL(sprintf("%s/mun_accessibility_gap_time.csv", output_dir)), row.names = FALSE)

# st_write(freg_stats_gap_sf, IMPT_URL(sprintf("%s/freg_stats_gap.gpkg", output_dir)), delete_dsn = TRUE)
# st_write(mun_stats_gap_sf,  IMPT_URL(sprintf("%s/mun_stats_gap.gpkg",  output_dir)), delete_dsn = TRUE)


# Accessibility Gap - Money -------------------------------------------------------

# Accessibility Gap - Money -------------------------------------------------------
# Files available under /data/IMPT/mobility_money_costs/:
#   grid_commuting_money_car.csv        - id_grid_origin, total_money (€, car), avg_tt, avg_distance, trips
#   grid_commuting_money_pt_single_fare.csv - id_grid_origin, total_money (€, PT single fare), trips
# total_money is already the trip-weighted mean cost per origin grid cell.
# Produced by 04_mobility_costs_money_car.R and 04_mobility_costs_money_pt.R.

money_dir <- IMPT_URL("mobility_money_costs")

costs_car_grid <- read.csv(file.path(money_dir, "grid_commuting_money_car.csv")) |>
    rename(cost_car = total_money, trips_car = trips) |>
    select(id_grid_origin, cost_car, trips_car) |>
    mutate(id_grid_origin = as.character(id_grid_origin))
summary(costs_car_grid) # max 39.9

costs_pt_grid <- read.csv(file.path(money_dir, "grid_commuting_money_pt_single_fare.csv")) |> # we are using single fare, can change for navegante
    rename(cost_pt = total_money, trips_pt = trips) |>
    select(id_grid_origin, cost_pt, trips_pt) |>
    mutate(id_grid_origin = as.character(id_grid_origin))
summary(costs_pt_grid) # max 7.64

# Grid-level cost gap -----------------------------------------------
# total_money is the trip-weighted mean cost at origin grid level.
# We use trips_car as the weight (matches the car itinerary coverage).
hex_stats_gap_money <- costs_car_grid |>
    left_join(costs_pt_grid, by = "id_grid_origin") |>
    filter(!is.na(cost_car)) |> # only grids where car cost was computed
    rename(grid_id = id_grid_origin) |>
  mutate(
    # Assign €10 ceiling if PT is unreachable
    cost_pt_for_gap = if_else(is.na(cost_pt), 10, cost_pt),
    
    # Absolute gap in Euros, positive = PT more expensive than car
    cost_gap = round(cost_pt_for_gap - cost_car, 2),
    trips = trips_car,
    
    # Relative cost index (0 = same cost)
    # Positive = PT is more expensive | Negative = PT is cheaper
    relative_gap_cost = ifelse(cost_pt_for_gap >= cost_car,
                                 (cost_pt_for_gap / cost_car) - 1,
                                 -((cost_car / cost_pt_for_gap) - 1)),
    relative_gap_cost = round(relative_gap_cost, 2)
  ) |>
  left_join(grid_freg_mun |> select(grid_id, freg_id, mun_id), by = "grid_id")

summary(hex_stats_gap_money)

# Viz Check
mapview(grid |> mutate(grid_id = as.character(id)) |> left_join(hex_stats_gap_money), zcol = "cost_gap")
mapview(grid |> mutate(grid_id = as.character(id)) |> left_join(hex_stats_gap_money), zcol = "relative_gap_cost")
# mapview(grid |> mutate(grid_id = as.character(id)) |> left_join(hex_stats_gap_money), zcol = "cost_car")
# mapview(grid |> mutate(grid_id = as.character(id)) |> left_join(hex_stats_gap_money), zcol = "cost_pt")

# at freguesia level: re-weight grid means by trips --------------------------------
freg_stats_gap_money <- hex_stats_gap_money |>
    filter(!is.na(freg_id)) |>
    group_by(freg_id) |>
    summarise(
        cost_car = round(weighted.mean(cost_car, trips, na.rm = TRUE),2),
        cost_pt = round(weighted.mean(cost_pt_for_gap, trips, na.rm = TRUE), 2),
        total_trips = round(sum(trips, na.rm = TRUE)),
        n_cells_no_pt_fare = sum(is.na(cost_pt)),
        .groups = "drop"
    ) |>
  mutate(
    freg_id   = as.character(freg_id),
    # Assign €10 ceiling if PT is unreachable
    cost_pt_for_gap = if_else(is.na(cost_pt), 10, cost_pt),
    
    # Absolute gap in Euros, positive = PT more expensive than car
    cost_gap = round(cost_pt_for_gap - cost_car, 2),
    
    # Relative cost index (0 = same cost)
    # Positive = PT is more expensive | Negative = PT is cheaper
    relative_gap_cost = ifelse(cost_pt_for_gap >= cost_car,
                               (cost_pt_for_gap / cost_car) - 1,
                               -((cost_car / cost_pt_for_gap) - 1)),
    relative_gap_cost = round(relative_gap_cost, 2)
  ) |>
    filter(!is.nan(cost_car))

# Viz Check
freg_stats_gap_money_sf <- freguesias |>
    select(dtmnfr, geom) |>
    left_join(freg_stats_gap_money, by = c("dtmnfr" = "freg_id"))

mapview(freg_stats_gap_money_sf, zcol = "cost_gap")
mapview(freg_stats_gap_money_sf, zcol = "relative_gap_cost")

# at municipio level: re-weight grid means by trips --------------------------------
mun_stats_gap_money <- hex_stats_gap_money |>
    filter(!is.na(mun_id)) |>
    group_by(mun_id) |>
    summarise(
        cost_car = round(weighted.mean(cost_car, trips, na.rm = TRUE),2),
        cost_pt = round(weighted.mean(cost_pt_for_gap, trips, na.rm = TRUE),2),
        total_trips = round(sum(trips, na.rm = TRUE)),
        n_cells_no_pt_fare = sum(is.na(cost_pt)),
        .groups = "drop"
    ) |>
  mutate(
    mun_id   = as.character(mun_id),
    # Assign €10 ceiling if PT is unreachable
    cost_pt_for_gap = if_else(is.na(cost_pt), 10, cost_pt),
    
    # Absolute gap in Euros, positive = PT more expensive than car
    cost_gap = round(cost_pt_for_gap - cost_car, 2),
    
    # Relative cost index (0 = same cost)
    # Positive = PT is more expensive | Negative = PT is cheaper
    relative_gap_cost = ifelse(cost_pt_for_gap >= cost_car,
                               (cost_pt_for_gap / cost_car) - 1,
                               -((cost_car / cost_pt_for_gap) - 1)),
    relative_gap_cost = round(relative_gap_cost, 2)
  ) |>
    filter(!is.nan(cost_car))

# Viz Check
mun_stats_gap_money_sf <- municipios |>
    select(municipio, geom) |>
    left_join(municipios_id) |> # municipios_id already loaded above
    left_join(mun_stats_gap_money, by = "mun_id")

mapview(mun_stats_gap_money_sf, zcol = "cost_gap")
mapview(mun_stats_gap_money_sf, zcol = "relative_gap_cost")

# Export ------------------------------------------------------------------
write.csv(hex_stats_gap_money, IMPT_URL(sprintf("%s/grid_accessibility_gap_money.csv", output_dir)), row.names = FALSE)
write.csv(freg_stats_gap_money, IMPT_URL(sprintf("%s/freg_accessibility_gap_money.csv", output_dir)), row.names = FALSE)
write.csv(mun_stats_gap_money, IMPT_URL(sprintf("%s/mun_accessibility_gap_money.csv", output_dir)), row.names = FALSE)
