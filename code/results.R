# All .csv files should be available as .csv with the same name!

# Accessibility  -------------------------------------------------

freguesia_accessibility = read.csv(IMPT_URL("/accessibility/r8/accessibility_freguesia.csv"))
municipio_accessibility = read.csv(IMPT_URL("/accessibility/r8/accessibility_municipio.csv"))
grid_accessibility = read.csv(IMPT_URL("/accessibility/r8/accessibility_grid.csv"))

# Mobility  -------------------------------------------------

freguesia_mobility_costs = read.csv(IMPT_URL("/mobility_costs/r8/mobility_freguesia.csv"))
municipio_mobility_costs = read.csv(IMPT_URL("/mobility_costs/r8/mobility_municipio.csv"))
grid_mobility_costs = read.csv(IMPT_URL("/mobility_costs/r8/mobility_grid.csv"))

freguesia_commuting = read.csv(IMPT_URL("/mobility_commuting/freguesia_commuting.csv"))
municipio_commuting = read.csv(IMPT_URL("/mobility_commuting/municipio_commuting.csv"))
grid_commuting = read.csv(IMPT_URL("/mobility_commuting/grid_commuting.csv"))

freguesia_transit = read.csv(IMPT_URL("/mobility_transit/freguesias_headways.csv"))
municipio_transit = read.csv(IMPT_URL("/mobility_transit/municipios_headways.csv"))
grid_transit = read.csv(IMPT_URL("/mobility_transit/grid_headways.csv"))

freguesia_transfers = read.csv(IMPT_URL("/mobility_commuting/freguesia_transfers.csv"))
municipio_transfers = read.csv(IMPT_URL("/mobility_commuting/municipio_transfers.csv"))
grid_transfers = read.csv(IMPT_URL("/mobility_commuting/grid_transfers.csv"))

# Affordability  -----------------------------------------------------------

freguesia_affordability_car = read.csv(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_car.csv"))
municipio_affordability_car = read.csv(IMPT_URL("/mobility_money_costs/municipio_commuting_money_car.csv"))
grid_affordability_car = read.csv(IMPT_URL("/mobility_money_costs/grid_commuting_money_car.csv"))

freguesia_affordability_pt_single_fare = read.csv(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_pt_single_fare.csv"))
municipio_affordability_pt_single_fare = read.csv(IMPT_URL("/mobility_money_costs/municipio_commuting_money_pt_single_fare.csv"))
grid_affordability_pt_single_fare = read.csv(IMPT_URL("/mobility_money_costs/grid_commuting_money_pt_single_fare.csv"))

freguesia_affordability_pt_pass = read.csv(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_pt_pass_fare.csv"))
municipio_affordability_pt_pass = read.csv(IMPT_URL("/mobility_money_costs/municipio_commuting_money_pt_pass_fare.csv"))
grid_affordability_pt_pass = read.csv(IMPT_URL("/mobility_money_costs/grid_commuting_money_pt_pass_fare.csv"))
