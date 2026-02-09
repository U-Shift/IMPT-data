library(gtfstools)
library(mapview)

# Get PT stops
gtfs_paths <- list.files(IMPT_URL("/gtfs/processed"), pattern="\\.zip$" , full.names = TRUE)
all_stops <- list()
for (i in gtfs_paths) {
  gtfs <- read_gtfs(i)
  stops_sf <- stops_as_sf(gtfs$stops)
  all_stops[[i]] <- stops_sf
}
mapview(all_stops)
#CM_gtfs <- read_gtfs(IMPT_URL("/gtfs/processed/gtfs_carris_metropolitana.zip"))
#  CM_stops_sf <- stops_as_sf(CM_gtfs$stops)
#  mapview(CM_stops_sf)
  