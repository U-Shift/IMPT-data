# All .csv files should be available as .csv with the same name!

DATA_LOCATION = "/data/IMPT" # When running at server.ushift.pt, use server local data

IMPT_URL = function(path) {
  # If does not start with "/", add it
  if (!startsWith(path, "/")) {
    path = paste0("/", path)
  }
  local_path = sprintf("%s%s", DATA_LOCATION, path)
  # If data location starts with "http", add api key to url
  if (startsWith(DATA_LOCATION, "http")) {
    # If API_KEY empty or not defined, throw error
    if (API_KEY == "") {
      stop("IMPT_DATA_KEY env var is not defined. Please set it using usethis::edit_r_environ() and restart R.")
    }
    return(sprintf("%s%s?key=%s", DATA_LOCATION, path, API_KEY))
  } else {
    # If local folder does not exist, create, recursively if needed
    if (!dir.exists(dirname(local_path))) {
      dir.create(dirname(local_path), recursive = TRUE)
    }
  }
  # Otherwise, return local path
  return(local_path)
}

# Accessibility  -------------------------------------------------

freguesia_accessibility <- read.csv(IMPT_URL("/accessibility/r8/accessibility_freguesia.csv"))
municipio_accessibility <- read.csv(IMPT_URL("/accessibility/r8/accessibility_municipio.csv"))
grid_accessibility <- read.csv(IMPT_URL("/accessibility/r8/accessibility_grid.csv"))

# Mobility  -------------------------------------------------

freguesia_mobility_costs <- read.csv(IMPT_URL("/mobility_costs/r8/mobility_freguesia.csv"))
municipio_mobility_costs <- read.csv(IMPT_URL("/mobility_costs/r8/mobility_municipio.csv"))
grid_mobility_costs <- read.csv(IMPT_URL("/mobility_costs/r8/mobility_grid.csv"))

freguesia_commuting <- read.csv(IMPT_URL("/mobility_commuting/freguesia_commuting.csv"))
municipio_commuting <- read.csv(IMPT_URL("/mobility_commuting/municipio_commuting.csv"))
grid_commuting <- read.csv(IMPT_URL("/mobility_commuting/grid_commuting.csv"))

freguesia_transit <- read.csv(IMPT_URL("/mobility_transit/freguesias_headways.csv"))
municipio_transit <- read.csv(IMPT_URL("/mobility_transit/municipios_headways.csv"))
grid_transit <- read.csv(IMPT_URL("/mobility_transit/grid_headways.csv"))

freguesia_transfers <- read.csv(IMPT_URL("/mobility_commuting/freguesia_transfers.csv"))
municipio_transfers <- read.csv(IMPT_URL("/mobility_commuting/municipio_transfers.csv"))
grid_transfers <- read.csv(IMPT_URL("/mobility_commuting/grid_transfers.csv"))

freguesia_stop_coverage <- read.csv(IMPT_URL("/mobility/freguesias_stops_coverage.csv"))
municipio_stop_coverage <- read.csv(IMPT_URL("/mobility/municipios_stops_coverage.csv"))
grid_stop_coverage <- read.csv(IMPT_URL("/mobility/grid_stops_coverage.csv"))

freguesia_shared_mobility <- read.csv(IMPT_URL("/mobility/freguesias_shared_mobility.csv"))
municipio_shared_mobility <- read.csv(IMPT_URL("/mobility/municipios_shared_mobility.csv"))
grid_shared_mobility <- read.csv(IMPT_URL("/mobility/grid_shared_mobility.csv"))

freguesia_mobility_infrastructure <- read.csv(IMPT_URL("/mobility/freguesias_infrastructure_ratio.csv"))
municipio_mobility_infrastructure <- read.csv(IMPT_URL("/mobility/municipios_infrastructure_ratio.csv"))
grid_mobility_infrastructure <- read.csv(IMPT_URL("/mobility/grid_infrastructure_ratio.csv"))

# Affordability  -----------------------------------------------------------

freguesia_affordability_car <- read.csv(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_car.csv"))
municipio_affordability_car <- read.csv(IMPT_URL("/mobility_money_costs/municipio_commuting_money_car.csv"))
grid_affordability_car <- read.csv(IMPT_URL("/mobility_money_costs/grid_commuting_money_car.csv"))

freguesia_affordability_pt_single_fare <- read.csv(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_pt_single_fare.csv"))
municipio_affordability_pt_single_fare <- read.csv(IMPT_URL("/mobility_money_costs/municipio_commuting_money_pt_single_fare.csv"))
grid_affordability_pt_single_fare <- read.csv(IMPT_URL("/mobility_money_costs/grid_commuting_money_pt_single_fare.csv"))

freguesia_affordability_pt_pass <- read.csv(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_pt_pass_fare.csv"))
municipio_affordability_pt_pass <- read.csv(IMPT_URL("/mobility_money_costs/municipio_commuting_money_pt_pass_fare.csv"))
grid_affordability_pt_pass <- read.csv(IMPT_URL("/mobility_money_costs/grid_commuting_money_pt_pass_fare.csv"))

freguesia_affordability_composite <- read.csv(IMPT_URL("/affordability/affordability_freguesia_composite.csv"))
municipio_affordability_composite <- read.csv(IMPT_URL("/affordability/affordability_municipio_composite.csv"))
grid_affordability_composite <- read.csv(IMPT_URL("/affordability/affordability_grid_composite.csv"))


# Safety ------------------------------------------------------------------

freguesia_safety <- read.csv(IMPT_URL("/safety/accidents_by_freguesia_5years.csv"))
municipio_safety <- read.csv(IMPT_URL("/safety/accidents_by_municipio_5years.csv"))
grid_safety <- read.csv(IMPT_URL("/safety/accidents_by_grid_5years.csv"))

freguesia_safety_inner <- read.csv(IMPT_URL("/safety/accidents_by_freguesia_5years_dentrolocalidades.csv"))
municipio_safety_inner <- read.csv(IMPT_URL("/safety/accidents_by_municipio_5years_dentrolocalidades.csv"))
grid_safety_inner <- read.csv(IMPT_URL("/safety/accidents_by_grid_5years_dentrolocalidades.csv"))


# Additional indicators ---------------------------------------------------

freguesia_pois <- read.csv(IMPT_URL("/pois/freguesias_pois.csv"))
municipio_pois <- read.csv(IMPT_URL("/pois/municipios_pois.csv"))
grid_pois <- read.csv(IMPT_URL("/pois/grid_pois.csv"))

freguesia_modal_share_census <- read.csv(IMPT_URL("/census2021/census_modal_share_parish.csv"))
municipio_modal_share_census <- read.csv(IMPT_URL("/census2021/census_modal_share_municipality.csv"))
grid_modal_share_census <- read.csv(IMPT_URL("/census2021/census_modal_share_grid.csv"))

freguesia_modal_share_imob <- read.csv(IMPT_URL("/trips/imob_modal_share_freg.csv"))
municipio_modal_share_imob <- read.csv(IMPT_URL("/trips/imob_modal_share_mun.csv"))
grid_modal_share_imob <- read.csv(IMPT_URL("/trips/imob_modal_share_grid.csv"))

freguesia_veh_ownership <- read.csv(IMPT_URL("/imob/imob_vehicles_freg.csv"))
municipio_veh_ownership <- read.csv(IMPT_URL("/imob/imob_vehicles_mun.csv"))
grid_veh_ownership <- read.csv(IMPT_URL("/imob/imob_vehicles_grid.csv"))

freguesia_census_income <- read.csv(IMPT_URL("/landuse/freguesias_income_housing_gini.csv"))
municipio_census_income <- read.csv(IMPT_URL("/landuse/municipios_income_housing_gini.csv"))
grid_census_income <- read.csv(IMPT_URL("/landuse/grid_income_housing_gini.csv"))

freguesia_census_landuse <- read.csv(IMPT_URL("/landuse/landuse_freguesias.csv"))
municipio_census_landuse <- read.csv(IMPT_URL("/landuse/landuse_municipios.csv"))
grid_census_landuse <- read.csv(IMPT_URL("/landuse/landuse_grid.csv"))

freguesia_access_gap_time <- read.csv(IMPT_URL("/mobility_commuting/freg_accessibility_gap_time.csv"))
municipio_access_gap_time <- read.csv(IMPT_URL("/mobility_commuting/mun_accessibility_gap_time.csv"))
grid_access_gap_time <- read.csv(IMPT_URL("/mobility_commuting/grid_accessibility_gap_time.csv"))

freguesia_access_gap_money <- read.csv(IMPT_URL("/mobility_commuting/freg_accessibility_gap_money.csv"))
municipio_access_gap_money <- read.csv(IMPT_URL("/mobility_commuting/mun_accessibility_gap_money.csv"))
grid_access_gap_money <- read.csv(IMPT_URL("/mobility_commuting/grid_accessibility_gap_money.csv"))

