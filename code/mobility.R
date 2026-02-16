# Nota: Alguns passos deste código demoram muito tempo a correr
# (nomeadamente os relacionados com a rede viária e pedonal total da AML)
# seria útil arranjar maneira de agregar mais os dados do OSM
# ou na road_network do ficheiro data_load (= st_read(IMPT_URL("/geo/IMPT_Road_network.gpkg")) 
# antes de correr tudo

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


# Roads ----
  # Get all road infrastructure OSM data for AML
osm_roads <- opq(bbox = "Área Metropolitana de Lisboa, Portugal") |>
  # Highways with tags "service", "track" and "road" are excluded
  add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", 
                  "tertiary", "unclassified", "residential", "motorway_link", "trunk_link",
                  "primary_link", "secondary_link", "tertiary_link", "living_street")
  ) |>
  osmdata_sf()
aml_roads <- osm_roads$osm_lines |>
  st_as_sf()
  # Remove unnecessary columns
aml_roads <- aml_roads |>
  select(osm_id, name, highway, geometry)
#mapview(aml_roads)

# Disaggregate and measure pedpath length by Freguesia
roads_by_freguesia <- st_join(aml_roads, freguesias, left = FALSE)
roads_by_freguesia$length_segment <- st_length(roads_by_freguesia)
road_length_by_freguesia <- roads_by_freguesia |>
  group_by(freguesia) |>
  summarise(road_length = sum(length_segment))
road_length_by_freguesia <- road_length_by_freguesia |>
  st_drop_geometry()
freguesias_by_road <- freguesias |>
  left_join(road_length_by_freguesia, by = "freguesia") |>
  mutate(road_length = ifelse(is.na(road_length), 0, road_length))
mapview(freguesias_by_road, zcol = "road_length")


# Pedestrians ----
  # Get OSM pedestrian infrastructure data for AML
osm_pedpaths <- opq(bbox = "Área Metropolitana de Lisboa, Portugal") |>
  add_osm_features(list(
    # Residential streets are included due to them not having 
    # path or sidewalk tags in OSM, but being mostly walkable.
    # Separate footpaths
    "highway" = c("footway", "residential", "pedestrian", "steps"),
    "footway" = c("sidewalk", "crossing", "path", "platform", "corridor", "alley", "track"),
    # Roads with sidewalk tags
    "sidewalk" = c("both", "left", "right")
    )) |>
  osmdata_sf()
aml_pedpaths <- osm_pedpaths$osm_lines |>
  st_as_sf()
  # Remove unnecessary columns
aml_pedpaths <- aml_pedpaths |>
  select(osm_id, name, highway, sidewalk, geometry)
#mapview(aml_pedpaths)

# Disaggregate and measure pedpath length by Freguesia
pedpaths_by_freguesia <- st_join(aml_pedpaths, freguesias, left = FALSE)
pedpaths_by_freguesia$length_segment <- st_length(pedpaths_by_freguesia)
pedpath_length_by_freguesia <- pedpaths_by_freguesia |>
  group_by(freguesia) |>
  summarise(pedpath_length = sum(length_segment))
pedpath_length_by_freguesia <- pedpath_length_by_freguesia |>
  st_drop_geometry()
freguesias_by_pedpath <- freguesias |>
  left_join(pedpath_length_by_freguesia, by = "freguesia") |>
  mutate(pedpath_length = ifelse(is.na(pedpath_length), 0, pedpath_length))
#mapview(freguesias_by_pedpath, zcol = "pedpath_length")


# Bicycles ----
  # Get OSM cycleway data for AML
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
  # Remove unnecessary columns
aml_cycleways <- aml_cycleways |>
  select(osm_id, name, bicycle, highway, geometry)
#mapview(aml_cycleways)

  # Disaggregate and measure cycleway length by Freguesia
cycleways_by_freguesia <- st_join(aml_cycleways, freguesias, left = FALSE)
cycleways_by_freguesia$length_segment <- st_length(cycleways_by_freguesia)
cycleway_length_by_freguesia <- cycleways_by_freguesia |>
  group_by(freguesia) |>
  summarise(cycleway_length = sum(length_segment))
cycleway_length_by_freguesia <- cycleway_length_by_freguesia |>
  st_drop_geometry()
freguesias_by_cycleway <- freguesias |>
  left_join(cycleway_length_by_freguesia, by = "freguesia") |>
  mutate(cycleway_length = ifelse(is.na(cycleway_length), 0, cycleway_length))
#mapview(freguesias_by_cycleway, zcol = "cycleway_length")


# Compute ratio of pedpath/cycleway to roads ----
freguesias_by_infrastructure <- freguesias |>
  left_join(road_length_by_freguesia, by = "freguesia") |>
  left_join(pedpath_length_by_freguesia, by = "freguesia") |>
  left_join(cycleway_length_by_freguesia, by = "freguesia") |>
  mutate(
    road_length = ifelse(is.na(road_length), 0, road_length),
    pedpath_length = ifelse(is.na(pedpath_length), 0, pedpath_length),
    cycleway_length = ifelse(is.na(cycleway_length), 0, cycleway_length),
    pedpath_to_road_ratio = ifelse(road_length > 0, pedpath_length / road_length, NA),
    cycleway_to_road_ratio = ifelse(road_length > 0, cycleway_length / road_length, NA)
  )
mapview(freguesias_by_infrastructure, zcol = "pedpath_to_road_ratio")
mapview(freguesias_by_infrastructure, zcol = "cycleway_to_road_ratio")



# Next steps:
# 1. Compute continuity of walking/cycling infrastructure 
# (use Speedwalk? (https://a-b-street.github.io/speedwalk/))
# 2. Compute quality of cycling infrastructure
