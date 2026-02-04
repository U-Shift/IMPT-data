# Load relevant data after preparation
# see data_prep-R to check hot it was prepared

options(java.parameters = '-Xmx32G') # RAM to 16GB
library(r5r)
library(dplyr)
library(sf)
library(tidyr)

# Geo ---------------------------------------------------------------------

# Polygons administrative
freguesias = st_read("/data/IMPT/geo/freguesias_2024_unique.gpkg")
municipios = st_read("/data/IMPT/geo/municipios_2024.gpkg")
limit = st_read("/data/IMPT/geo/municipios_union_2024.geojson")
limit_bbox = st_read("/data/IMPT/geo/municipios_union_bbox_2024.geojson")

# Conversion freguesias
conversion_dicofre = read.csv("useful_data/dicofre_16_24_conversion.csv")
conversion_dicofre_all = readRDS("useful_data/dicofre_16_24_conversion_full.Rds")
conversion_dicofre_weight = readRDS("useful_data/dicofre_16_24_conversion_full_with_weights.Rds")

# Road network
road_network = st_read("/data/IMPT/geo/IMPT_Road_network.gpkg") # all
road_network_main = st_read("/data/IMPT/geo/road_network_main.gpkg") # 1 to 4 level
road_network_base = st_read("/data/IMPT/geo/road_network_base.gpkg") # 1 to 3 level

# ODs
trips_freguesias_2024 = readRDS("/data/IMPT/trips/TRIPSmode_freguesias_2024.Rds")
od_freguesias_jittered200 = st_read("/data/IMPT/trips/od_freguesias_jittered_2024.gpkg") # lines
od_freguesias_jittered_OR_geo = st_read("/data/IMPT/trips/od_freguesias_jittered200_OR.gpkg") # points origin
od_freguesias_jittered_DE_geo = st_read("/data/IMPT/trips/od_freguesias_jittered200_DE.gpkg") # points destination

# Census points
census = st_read("/data/IMPT/geo/census24_points.gpkg")

# POIs
pois = st_read("/data/IMPT/geo/pois_osm2024.gpkg")
pois_healthcare = st_read("/data/IMPT/pois/healthcare.gpkg")
# pois_pharmacies = st_read("/data/IMPT/pois/pharmacies.gpkg")
# pois_schools = st_read("/data/IMPT/pois/schools.gpkg")

# Grid
grid = st_read("/data/IMPT/geo/grelha_tml_d500.gpkg")

# r5r
r5r_core = r5r::setup_r5("/data/IMPT/geo/r5r/", verbose = FALSE)


# Statistic Data ----------------------------------------------------------


# r5r::stop_r5(r5r_core)
# rJava::.jgc(R.gc = TRUE)

