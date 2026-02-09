library(gtfstools)
library(mapview)
library(osmdata)

# Get PT stops
gtfs_paths <- list.files(IMPT_URL("/gtfs/processed"), pattern="\\.zip$" , full.names = TRUE)
all_stops <- list()
for (i in gtfs_paths) {
  gtfs <- read_gtfs(i)
  stops_sf <- stops_as_sf(gtfs$stops)
  all_stops[[i]] <- stops_sf
}
mapview(all_stops)


# Get cycleways from OSM
osm_cycleways <- opq(bbox = "Área Metropolitana de Lisboa, Portugal") |>
  add_osm_feature(key = "highway", value = "cycleway") |>
  osmdata_sf()
aml_cycleways <- osm_cycleways$osm_lines |>
  st_as_sf()
mapview(aml_cycleways)
