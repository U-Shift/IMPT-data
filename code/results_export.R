# This script generates simplified versions of each dimension, with only the indicators used to compute the IMPT

# 1. Load aggregated data  ------------------------------------------------
# >> When adding new data, make sure it has ben aggregated previously by dashboard_export.R!

data_dir = "dashboard_data"
grid_aggregated = st_read(IMPT_URL(paste(data_dir, "grid_aggregated.geojson", sep="/")))
freguesias_aggregated = st_read(IMPT_URL(paste(data_dir, "freguesias_aggregated.geojson", sep="/")))
municipios_aggregated = st_read(IMPT_URL(paste(data_dir, "municipios_aggregated.geojson", sep="/")))
mun_nuts = read.csv("useful_data/mun_nuts.csv") 

# UTILS
output_dir = "results_data"
store_by_mode = function(data, cols, data_name, destination_folder) {
  modes = list("walk", "bike", "car", "transit")
  write.csv(
    data |> select(all_of(cols), contains(unlist(modes))), 
    IMPT_URL(paste(destination_folder, paste0(data_name, ".csv"), sep="/")), 
    row.names = FALSE
  )

  for (mode in modes) {
    data_mode = data |> 
      # Select pre-defined cols + those that contain the mode
      select(all_of(cols), contains(mode))
    
    var_name = paste(data_name, mode, sep="_")
    assign(var_name, data_mode)
    
    write.csv(data_mode, IMPT_URL(paste(destination_folder, paste0(var_name, ".csv"), sep="/")), row.names = FALSE)
  }
}


# 2. Accessibility --------------------------------------------------------
select_impt_cols_accessibility = function(data) {
  return (
    data |> 
      select(
        id, region_id, 
        # Number of opportunities reachable within threshold (travel time)
        # - Healthcare (15 Walk-Cycle; 30 min PT-CAR)
        access_health_walk_15min_residents, access_health_bike_15min_residents, access_health_transit_2t_30min_residents, access_health_car_30min_residents,
        # - Basic Education (15 Walk-Cycle; 30 min PT-CAR)
        access_schools_primary_bike_15min_kids, access_schools_primary_walk_15min_kids, access_schools_primary_transit_2t_30min_kids, access_schools_primary_car_30min_kids,
        # - Green Spaces (15-30 min Walk-Cycle; 5-10 min PT-CAR)
        access_greenspaces_walk_15min_residents, access_greenspaces_bike_15min_residents, access_greenspaces_transit_2t_5min_residents, access_greenspaces_car_5min_residents,
        # - Recreation (15-30 min Walk-Cycle; 5-10 min PT-CAR)
        access_recreation_walk_15min_residents, access_recreation_bike_15min_residents, access_recreation_transit_2t_5min_residents, access_recreation_car_5min_residents,
        # - Groceries and basic shopping (15 min all modes)
        access_groceries_walk_15min_residents, access_groceries_bike_15min_residents, access_groceries_transit_2t_15min_residents, access_groceries_car_15min_residents,
        # - Jobs (30-45 min all modes)
        access_jobs_walk_30min_active, access_jobs_bike_30min_active, access_jobs_transit_2t_45min_active, access_jobs_car_45min_active,
        
        # Travel time to first n opportunities
        # - Healthcare (1st opportunity)
        mobility_cost_health_walk_n1_residents, mobility_cost_health_bike_n1_residents, mobility_cost_health_transit_2t_n1_residents, mobility_cost_health_car_n1_residents,
        # - Basic Education (1st opportunity)
        mobility_cost_schools_primary_walk_n1_kids, mobility_cost_schools_primary_bike_n1_kids, mobility_cost_schools_primary_transit_2t_n1_kids, mobility_cost_schools_primary_car_n1_kids,
        # - Green Spaces (1st opportunity)
        mobility_cost_greenspaces_walk_n1_residents, mobility_cost_greenspaces_bike_n1_residents, mobility_cost_greenspaces_transit_2t_n1_residents, mobility_cost_greenspaces_car_n1_residents,
        # - Recreation (3 opportinities)
        mobility_cost_recreation_walk_n3_residents, mobility_cost_recreation_bike_n3_residents, mobility_cost_recreation_transit_2t_n3_residents, mobility_cost_recreation_car_n3_residents,
        # - Groceries and basic shopping (3 opportunities)
        mobility_cost_groceries_walk_n3_residents, mobility_cost_groceries_bike_n3_residents, mobility_cost_groceries_transit_2t_n3_residents, mobility_cost_groceries_car_n3_residents,
        # - Jobs (3 opportunities)
        mobility_cost_jobs_walk_n3_active, mobility_cost_jobs_bike_n3_active, mobility_cost_jobs_transit_2t_n3_active, mobility_cost_jobs_car_n3_active
      ) |> 
      st_drop_geometry()
  )
}

freguesia_accessibility = select_impt_cols_accessibility(freguesias_aggregated) |>
  rename(dtmnfr = id, nuts=region_id) 
names(freguesia_accessibility)
municipio_accessibility = select_impt_cols_accessibility(municipios_aggregated) |>
  mutate(id=as.integer(id)) |>
  left_join(mun_nuts |> select(id, municipio), by="id") |>
  rename(nuts=region_id)
names(municipio_accessibility)

store_by_mode(freguesia_accessibility, c("dtmnfr", "nuts"), "freguesia_accessibility", output_dir)
store_by_mode(municipio_accessibility, c("municipio", "nuts"), "municipio_accessibility", output_dir)


# 3. Mobility -------------------------------------------------------------
select_impt_cols_mobility = function(data) {
  return (
    data |> 
      select(
        id, region_id, 
        # Travel time (peak)
        mobility_commuting_avg_tt_bike, mobility_commuting_avg_tt_walk, mobility_commuting_avg_tt_transit_2t_120m_15w, mobility_commuting_avg_tt_car,
        # Number of transfers for key destinations (transit)
        mobility_transit_commuting_mean_transfers,
        # PT Waiting Times
        mobility_transit_weighted_waiting_time_peak,
        # Night/weekend service availability
        mobility_transit_weighted_frequency_reduction_night, mobility_transit_weighted_frequency_reduction_weekend
      ) |> 
      st_drop_geometry()
  )
}

freguesia_mobility = select_impt_cols_mobility(freguesias_aggregated) |>
  rename(dtmnfr = id, nuts=region_id) 
names(freguesia_mobility)
municipio_mobility = select_impt_cols_mobility(municipios_aggregated) |>
  mutate(id=as.integer(id)) |>
  left_join(mun_nuts |> select(id, municipio), by="id") |>
  rename(nuts=region_id)
names(municipio_mobility)

store_by_mode(freguesia_mobility, c("dtmnfr", "nuts"), "freguesia_mobility", output_dir)
store_by_mode(municipio_mobility, c("municipio", "nuts"), "municipio_mobility", output_dir)


# 4. Affordability --------------------------------------------------------
select_impt_cols_affordability = function(data) {
  return (
    data |> 
      select(
        id, region_id, 
        # Affordability (total money cost of commuting)
        affordability_car_total_money, affordability_pt_single_fare_total_money, affordability_pt_pass_total_money
      ) |> 
      st_drop_geometry() |>
      # In col names, replace "_pt_", with "_transit_"
      rename_with(~ gsub("_pt_", "_transit_", .x))
  )
}

freguesia_affordability = select_impt_cols_affordability(freguesias_aggregated) |>
  rename(dtmnfr = id, nuts=region_id)
names(freguesia_affordability)
municipio_affordability = select_impt_cols_affordability(municipios_aggregated) |>
  mutate(id=as.integer(id)) |>
  left_join(mun_nuts |> select(id, municipio), by="id") |>
  rename(nuts=region_id)
names(municipio_affordability)

store_by_mode(freguesia_affordability, c("dtmnfr", "nuts"), "freguesia_affordability", output_dir)
store_by_mode(municipio_affordability, c("municipio", "nuts"), "municipio_affordability", output_dir)
