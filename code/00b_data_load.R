# Load relevant data after preparation
# see data_prep-R to check hot it was prepared

# Geo ---------------------------------------------------------------------

# Polygons administrative
freguesias <- impt_read("/geo/freguesias_2024_unique.gpkg")
municipios <- impt_read("/geo/municipios_2024.gpkg")
limit <- impt_read("/geo/municipios_union_2024.geojson")
limit_bbox <- impt_read("/geo/municipios_union_bbox_2024.geojson")

# Conversion freguesias
conversion_dicofre <- read.csv("useful_data/dicofre_16_24_conversion.csv")
conversion_dicofre_all <- readRDS("useful_data/dicofre_16_24_conversion_full.Rds")
conversion_dicofre_weight <- readRDS("useful_data/dicofre_16_24_conversion_full_with_weights.Rds")

# Road network
# road_network = impt_read("/geo/IMPT_Road_network.gpkg") # all
road_network_main <- impt_read("/geo/road_network_main.gpkg") # 1 to 4 level
road_network_base <- impt_read("/geo/road_network_base.gpkg") # 1 to 3 level

# Census points
census <- impt_read("/geo/census24_points.gpkg")

# POIs
pois <- impt_read("/geo/pois_osm2024.gpkg")

# Grid
grid_tml <- impt_read("/geo/grelha_tml_d500.gpkg")
# r8
grid <- impt_read("/geo/grelha_h3_r8.gpkg")
h3_index <- impt_read("/geo/grelha_h3_r8_index.Rds")
points_h3 <- impt_read("/geo/grelha_h3_r8_centroids.gpkg")

# Statistic Data ----------------------------------------------------------
