# All .gpkg files should be available as .csv with the same name!

# Accessibility  -------------------------------------------------

freguesia_accessibility = st_read(IMPT_URL("/accessibility/r8/accessibility_freguesia.gpkg"))
municipio_accessibility = st_read(IMPT_URL("/accessibility/r8/accessibility_municipio.gpkg"))
grid_accessibility = st_read(IMPT_URL("/accessibility/r8/accessibility_grid.gpkg"))

# Mobility  -------------------------------------------------
freguesia_mobility_costs = st_read(IMPT_URL("/mobility_costs/r8/mobility_freguesia.gpkg"))
municipio_mobility_costs = st_read(IMPT_URL("/mobility_costs/r8/mobility_municipio.gpkg"))
grid_mobility_costs = st_read(IMPT_URL("/mobility_costs/r8/mobility_grid.gpkg"))

freguesia_commuting = st_read(IMPT_URL("/mobility_commuting/freguesia_commuting.gpkg"))
municipio_commuting = st_read(IMPT_URL("/mobility_commuting/municipio_commuting.gpkg"))
grid_commuting = st_read(IMPT_URL("/mobility_commuting/grid_commuting.gpkg"))

freguesia_transit = st_read(IMPT_URL("/mobility_transit/freguesias_headways.gpkg"))
municipio_transit = st_read(IMPT_URL("/mobility_transit/municipios_headways.gpkg"))
grid_transit = st_read(IMPT_URL("/mobility_transit/grid_headways.gpkg"))

freguesia_transfers = st_read(IMPT_URL("/mobility_commuting/freguesia_transfers.gpkg"))
municipio_transfers = st_read(IMPT_URL("/mobility_commuting/municipio_transfers.gpkg"))
grid_transfers = st_read(IMPT_URL("/mobility_commuting/grid_transfers.gpkg"))


# Affordability  -----------------------------------------------------------

freguesia_affordability_car = st_read(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_car.gpkg"))
municipio_affordability_car = st_read(IMPT_URL("/mobility_money_costs/municipio_commuting_money_car.gpkg"))
grid_affordability_car = st_read(IMPT_URL("/mobility_money_costs/grid_commuting_money_car.gpkg"))

freguesia_affordability_pt_single_fare = st_read(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_pt_single_fare.gpkg"))
municipio_affordability_pt_single_fare = st_read(IMPT_URL("/mobility_money_costs/municipio_commuting_money_pt_single_fare.gpkg"))
grid_affordability_pt_single_fare = st_read(IMPT_URL("/mobility_money_costs/grid_commuting_money_pt_single_fare.gpkg"))
# mapview(freguesia_affordability_pt_single_fare, zcol = "total_money") + mapview(grid_affordability_pt_single_fare, zcol = "total_money")

freguesia_affordability_pt_pass = st_read(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_pt_pass_fare.gpkg"))
municipio_affordability_pt_pass = st_read(IMPT_URL("/mobility_money_costs/municipio_commuting_money_pt_pass_fare.gpkg"))
grid_affordability_pt_pass = st_read(IMPT_URL("/mobility_money_costs/grid_commuting_money_pt_pass_fare.gpkg"))
# mapview(freguesia_affordability_pt_pass, zcol = "total_money") + mapview(grid_affordability_pt_pass, zcol = "total_money")