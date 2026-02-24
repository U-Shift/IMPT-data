# Accessibility  -------------------------------------------------

freguesia_accessibility = st_read(IMPT_URL("/accessibility/r8/accessibility_freguesia.gpkg"))
municipio_accessibility = st_read(IMPT_URL("/accessibility/r8/accessibility_municipio.gpkg"))
grid_accessibility = st_read(IMPT_URL("/accessibility/r8/accessibility_grid.gpkg"))

freguesia_mobility_costs = st_read(IMPT_URL("/mobility_costs/r8/mobility_freguesia.gpkg"))
municipio_mobility_costs = st_read(IMPT_URL("/mobility_costs/r8/mobility_municipio.gpkg"))
grid_mobility_costs = st_read(IMPT_URL("/mobility_costs/r8/mobility_grid.gpkg"))

freguesia_commuting = st_read(IMPT_URL("/mobility_commuting/freguesia_commuting.gpkg"))
municipio_commuting = st_read(IMPT_URL("/mobility_commuting/municipio_commuting.gpkg"))
grid_commuting = st_read(IMPT_URL("/mobility_commuting/grid_commuting.gpkg"))
