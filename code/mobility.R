library(gtfstools)
library(mapview)
library(osmdata)

# Get all AML PT stops ----
gtfs_paths <- list.files(IMPT_URL("/gtfs/processed"), pattern="\\.zip$" , full.names = TRUE)
all_stops <- list()
for (i in gtfs_paths) {
  gtfs <- read_gtfs(i)
  stops_sf <- stops_as_sf(gtfs$stops)
  all_stops[[i]] <- stops_sf
}
mapview(all_stops)


# Get cycleways from OSM ----
osm_cycleways <- opq(bbox = "Área Metropolitana de Lisboa, Portugal") |>
  add_osm_features(features = list(
    # Dedicated cycle paths
    "highway" = "cycleway",
    # Cycle lanes on roads
    "cycleway" = c("lane", "track", "opposite_lane", "opposite_track", "shared_lane", "share_busway"),
    "cycleway:left" = c("lane", "track", "shared_lane", "share_busway"),
    "cycleway:right" = c("lane", "track", "shared_lane", "share_busway"),
    "cycleway:both" = c("lane", "track", "shared_lane", "share_busway")
  )) |>
  osmdata_sf()
aml_cycleways <- osm_cycleways$osm_lines |>
  st_as_sf()
mapview(aml_cycleways)


# Get pedestrian paths from OSM ----
osm_pedpaths <- opq(bbox = "Área Metropolitana de Lisboa, Portugal") |>
  add_osm_features(list(
    # Separate footpaths. 
    # Residential streets are included due to them not including neither paths nor sidewalk tags but being mostly walkable.
    "highway" = c("footway", "residential", "pedestrian", "steps"),
    "footway" = c("sidewalk", "crossing", "path", "platform", "corridor", "alley", "track"),
    #Roads with sidewalk tags
    "sidewalk" = c("both", "left", "right")
    )) |>
  osmdata_sf()
aml_pedpaths <- osm_pedpaths$osm_lines |>
  st_as_sf()
mapview(aml_pedpaths)

