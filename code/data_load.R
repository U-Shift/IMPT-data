# Load relevant data after preparation
# see data_prep-R to check hot it was prepared

options(java.parameters = '-Xmx32G') # RAM to 16GB
library(r5r)
library(dplyr)
library(sf)
library(tidyr)


# Data location  ----------------------------------------------------------

# DATA_LOCATION = sprintf("%s" # When running at server.ushift.pt, use server local data
DATA_LOCATION = "https://impt.server.ushift.pt" # When running locally, get data from remote server
API_KEY = Sys.getenv("IMPT_DATA_KEY") # Set it using usethis::edit_r_environ(), followed by CTRL+F10

IMPT_URL = function(path) {
  # If data location starts with "http", add api key to url
  if (startsWith(DATA_LOCATION, "http")) {
    # If API_KEY empty or not defined, throw error
    if (API_KEY == "") {
      stop("IMPT_DATA_KEY env var is not defined. Please set it using usethis::edit_r_environ() and restart R.")
    }
    return(sprintf("%s%s?key=%s", DATA_LOCATION, path, API_KEY))
  }
  # Otherwise, return local path
  return(DATA_LOCATION)
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

download_remote_dir <- function(dir_url, local_folder) {
  # If does not exist, create it
  if (!dir.exists(local_folder)) {
    dir.create(local_folder, recursive = TRUE)
  }
  # Fetch nginx directory listing
  html <- readLines(dir_url, warn = FALSE)
  href_lines <- html[grepl("href=", html)]
  # Extract filenames
  files <- sub('.*href="([^"]+)".*', '\\1', href_lines)
  # Remove parent dir, trailing slashes, and query junk
  files <- files[
    files != "../" &
      !grepl("/$", files)
  ]
  # Download files
  for (f in files) {
    download.file(
      paste0(stringr::str_split(dir_url, "\\?")[[1]][[1]], f, "?", stringr::str_split(dir_url, "\\?")[[1]][[2]]),
      file.path(local_folder, f),
      mode = "wb"
    )
  }
  return(local_folder)
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
road_network = st_read(IMPT_URL("/geo/IMPT_Road_network.gpkg")) # all
road_network_main = st_read(IMPT_URL("/geo/road_network_main.gpkg")) # 1 to 4 level
road_network_base = st_read(IMPT_URL("/geo/road_network_base.gpkg")) # 1 to 3 level

# ODs
trips_freguesias_2024 = readRDS_remote(IMPT_URL("/trips/TRIPSmode_freguesias_2024.Rds"))
od_freguesias_jittered200 = st_read(IMPT_URL("/trips/od_freguesias_jittered_2024.gpkg")) # lines
od_freguesias_jittered_OR_geo = st_read(IMPT_URL("/trips/od_freguesias_jittered200_OR.gpkg")) # points origin
od_freguesias_jittered_DE_geo = st_read(IMPT_URL("/trips/od_freguesias_jittered200_DE.gpkg")) # points destination

# Census points
census = st_read(IMPT_URL("/geo/census24_points.gpkg"))

# POIs
pois = st_read(IMPT_URL("/geo/pois_osm2024.gpkg"))
pois_healthcare = st_read(IMPT_URL("/pois/healthcare.gpkg"))
# pois_pharmacies = st_read(IMPT_URL("/pois/pharmacies.gpkg"))
# pois_schools = st_read(IMPT_URL("/pois/schools.gpkg"))

# Grid
grid_tml = st_read(IMPT_URL("/geo/grelha_tml_d500.gpkg"))
grid = st_read(IMPT_URL("/geo/grelha_h3_r8.gpkg"))
h3_index = readRDS_remote(IMPT_URL("/geo/grelha_h3_r8_index.Rds"))
points_h3 = st_read(IMPT_URL("/geo/grelha_h3_r8_centroids.gpkg"))


# r5r
r5r_location = IMPT_URL("/geo/r5r/")
# If remote, download files to local before proceeding with r5r setup
# Only downloads first time, after that it uses local copy, so it is faster 
# Attention! If r5r updated on server, make sure to force download by deleting local folder "data/r5r" before running again
if (grepl("^http", r5r_location, ignore.case = TRUE)) {
  local_folder = "data/r5r"
  if (dir.exists(local_folder)) {
    r5r_location = local_folder
    message("Using local copy of r5r data. To force download, delete the folder 'data/r5r' and run again.")
  } else {
    message("Downloading r5r data from remote server. This may take a while the first time...")
    r5r_location = download_remote_dir(r5r_location, "data/r5r") # This takes a while for the first time
  }
}

r5r_network = r5r::build_network(r5r_location, verbose = FALSE)


# Attention! Stop here. Run the code below only when you have finished using r5r, to free up memory :)
r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)


# Statistic Data ----------------------------------------------------------


