# Accessibility  -------------------------------------------------

freguesia_accessibility <- impt_read("/accessibility/r8/accessibility_freguesia.csv")
municipio_accessibility <- impt_read("/accessibility/r8/accessibility_municipio.csv")
grid_accessibility <- impt_read("/accessibility/r8/accessibility_grid.csv")

# Mobility  -------------------------------------------------

freguesia_mobility_costs <- impt_read("/mobility_costs/r8/mobility_freguesia.csv")
municipio_mobility_costs <- impt_read("/mobility_costs/r8/mobility_municipio.csv")
grid_mobility_costs <- impt_read("/mobility_costs/r8/mobility_grid.csv")

freguesia_commuting <- impt_read("/mobility_commuting/freguesia_commuting.csv")
municipio_commuting <- impt_read("/mobility_commuting/municipio_commuting.csv")
grid_commuting <- impt_read("/mobility_commuting/grid_commuting.csv")

freguesia_transit <- impt_read("/mobility_transit/freguesias_headways.csv")
municipio_transit <- impt_read("/mobility_transit/municipios_headways.csv")
grid_transit <- impt_read("/mobility_transit/grid_headways.csv")

freguesia_transfers <- impt_read("/mobility_commuting/freguesia_transfers.csv")
municipio_transfers <- impt_read("/mobility_commuting/municipio_transfers.csv")
grid_transfers <- impt_read("/mobility_commuting/grid_transfers.csv")

freguesia_stop_coverage <- impt_read("/mobility/freguesias_stops_coverage.csv")
municipio_stop_coverage <- impt_read("/mobility/municipios_stops_coverage.csv")
grid_stop_coverage <- impt_read("/mobility/grid_stops_coverage.csv")

freguesia_shared_mobility <- impt_read("/mobility/freguesias_shared_mobility.csv")
municipio_shared_mobility <- impt_read("/mobility/municipios_shared_mobility.csv")
grid_shared_mobility <- impt_read("/mobility/grid_shared_mobility.csv")

freguesia_mobility_infrastructure <- impt_read("/mobility/freguesias_infrastructure_ratio.csv")
municipio_mobility_infrastructure <- impt_read("/mobility/municipios_infrastructure_ratio.csv")
grid_mobility_infrastructure <- impt_read("/mobility/grid_infrastructure_ratio.csv")

# Affordability  -----------------------------------------------------------

freguesia_affordability_car <- impt_read("/mobility_money_costs/freguesia_commuting_money_car.csv")
municipio_affordability_car <- impt_read("/mobility_money_costs/municipio_commuting_money_car.csv")
grid_affordability_car <- impt_read("/mobility_money_costs/grid_commuting_money_car.csv")

freguesia_affordability_pt_single_fare <- impt_read("/mobility_money_costs/freguesia_commuting_money_pt_single_fare.csv")
municipio_affordability_pt_single_fare <- impt_read("/mobility_money_costs/municipio_commuting_money_pt_single_fare.csv")
grid_affordability_pt_single_fare <- impt_read("/mobility_money_costs/grid_commuting_money_pt_single_fare.csv")

freguesia_affordability_pt_pass <- impt_read("/mobility_money_costs/freguesia_commuting_money_pt_pass_fare.csv")
municipio_affordability_pt_pass <- impt_read("/mobility_money_costs/municipio_commuting_money_pt_pass_fare.csv")
grid_affordability_pt_pass <- impt_read("/mobility_money_costs/grid_commuting_money_pt_pass_fare.csv")

freguesia_affordability_composite <- impt_read("/affordability/affordability_freguesia_composite.csv")
municipio_affordability_composite <- impt_read("/affordability/affordability_municipio_composite.csv")
grid_affordability_composite <- impt_read("/affordability/affordability_grid_composite.csv")


# Safety ------------------------------------------------------------------

freguesia_safety <- impt_read("/safety/accidents_by_freguesia_5years.csv")
municipio_safety <- impt_read("/safety/accidents_by_municipio_5years.csv")
grid_safety <- impt_read("/safety/accidents_by_grid_5years.csv")

freguesia_safety_inner <- impt_read("/safety/accidents_by_freguesia_5years_dentrolocalidades.csv")
municipio_safety_inner <- impt_read("/safety/accidents_by_municipio_5years_dentrolocalidades.csv")
grid_safety_inner <- impt_read("/safety/accidents_by_grid_5years_dentrolocalidades.csv")


# Additional indicators ---------------------------------------------------

freguesia_pois <- impt_read("/pois/freguesias_pois.csv")
municipio_pois <- impt_read("/pois/municipios_pois.csv")
grid_pois <- impt_read("/pois/grid_pois.csv")

freguesia_modal_share_census <- impt_read("/census2021/census_modal_share_parish.csv")
municipio_modal_share_census <- impt_read("/census2021/census_modal_share_municipality.csv")
grid_modal_share_census <- impt_read("/census2021/census_modal_share_grid.csv")

freguesia_modal_share_imob <- impt_read("/trips/imob_modal_share_freg.csv")
municipio_modal_share_imob <- impt_read("/trips/imob_modal_share_mun.csv")
grid_modal_share_imob <- impt_read("/trips/imob_modal_share_grid.csv")

freguesia_veh_ownership <- impt_read("/imob/imob_vehicles_freg.csv")
municipio_veh_ownership <- impt_read("/imob/imob_vehicles_mun.csv")
grid_veh_ownership <- impt_read("/imob/imob_vehicles_grid.csv")

freguesia_census_income <- impt_read("/landuse/freguesias_income_housing_gini.csv")
municipio_census_income <- impt_read("/landuse/municipios_income_housing_gini.csv")
grid_census_income <- impt_read("/landuse/grid_income_housing_gini.csv")

freguesia_census_landuse <- impt_read("/landuse/landuse_freguesias.csv")
municipio_census_landuse <- impt_read("/landuse/landuse_municipios.csv")
grid_census_landuse <- impt_read("/landuse/landuse_grid.csv")

freguesia_access_gap_time <- impt_read("/mobility_commuting/freg_accessibility_gap_time.csv")
municipio_access_gap_time <- impt_read("/mobility_commuting/mun_accessibility_gap_time.csv")
grid_access_gap_time <- impt_read("/mobility_commuting/grid_accessibility_gap_time.csv")

freguesia_access_gap_money <- impt_read("/mobility_commuting/freg_accessibility_gap_money.csv")
municipio_access_gap_money <- impt_read("/mobility_commuting/mun_accessibility_gap_money.csv")
grid_access_gap_money <- impt_read("/mobility_commuting/grid_accessibility_gap_money.csv")
