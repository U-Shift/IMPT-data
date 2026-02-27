# Load relevant data after preparation
# see data_prep-R to check hot it was prepared

library(dplyr)
library(sf)
library(tidyr)


# Data location  ----------------------------------------------------------

# DATA_LOCATION = "data"
DATA_LOCATION = "https://impt.server.ushift.pt" # When running locally, get data from remote server
DATA_LOCATION = "/data/IMPT" # When running at server.ushift.pt, use server local data
API_KEY = Sys.getenv("IMPT_DATA_KEY") # Set it using usethis::edit_r_environ(), followed by CTRL+F10

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

readRDS_remote <- function(file, quiet = TRUE) { # From https://stackoverflow.com/a/66874958
  if (grepl("^http", file, ignore.case = TRUE)) {
    # temp location
    file_local <- file.path(tempdir(), basename(file))
    # download the data set
    download.file(file, file_local, quiet = quiet, mode = "wb")
    file <- file_local
  }
  readRDS(file)
}

download_remote_file <- function(dir_url, filename, destinatin_folder) {
  download.file(
    paste0(stringr::str_split(r5r_location, "\\?")[[1]][[1]], filename, "?", stringr::str_split(r5r_location, "\\?")[[1]][[2]]),
    file.path(destinatin_folder, filename),
    mode = "wb"
  )
}

# Geo ---------------------------------------------------------------------

# Polygons administrative
freguesias = st_read(IMPT_URL("/geo/freguesias_2024_unique.gpkg"))
municipios = st_read(IMPT_URL("/geo/municipios_2024.gpkg"))
limit = st_read(IMPT_URL("/geo/municipios_union_2024.geojson"))
limit_bbox = st_read(IMPT_URL("/geo/municipios_union_bbox_2024.geojson"))

# Conversion freguesias
conversion_dicofre = read.csv("useful_data/dicofre_16_24_conversion.csv")
conversion_dicofre_all = readRDS("useful_data/dicofre_16_24_conversion_full.Rds")
conversion_dicofre_weight = readRDS("useful_data/dicofre_16_24_conversion_full_with_weights.Rds")

# Road network
# road_network = st_read(IMPT_URL("/geo/IMPT_Road_network.gpkg")) # all
road_network_main = st_read(IMPT_URL("/geo/road_network_main.gpkg")) # 1 to 4 level
road_network_base = st_read(IMPT_URL("/geo/road_network_base.gpkg")) # 1 to 3 level
# active_infra_ratio = readRDS(IMPT_URL("/mobility/freguesias_infrastructure_ratio.rds")) # not working

# Census points
census = st_read(IMPT_URL("/geo/census24_points.gpkg"))

# POIs
pois = st_read(IMPT_URL("/geo/pois_osm2024.gpkg"))

# Grid
grid_tml = st_read(IMPT_URL("/geo/grelha_tml_d500.gpkg"))
# r8
grid = st_read(IMPT_URL("/geo/grelha_h3_r8.gpkg"))
h3_index = readRDS_remote(IMPT_URL("/geo/grelha_h3_r8_index.Rds"))
points_h3 = st_read(IMPT_URL("/geo/grelha_h3_r8_centroids.gpkg"))

# Statistic Data ----------------------------------------------------------


