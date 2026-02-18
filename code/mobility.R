# Nota: Alguns passos deste código demoram muito tempo a correr
# (nomeadamente os relacionados com a rede viária e pedonal total da AML)
# seria útil arranjar maneira de agregar mais os dados do OSM
# ou na road_network do ficheiro data_load (= st_read(IMPT_URL("/geo/IMPT_Road_network.gpkg")) 
# antes de correr tudo

library(gtfstools)
library(mapview)
library(osmdata)
library(igraph)
library(tidygraph)
library(sfnetworks)

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
osm_roads <- opq(bbox = municipios |> sf::st_bbox()) |>
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

# Disaggregate and measure road length by Freguesia
roads_by_freguesia <- st_join(aml_roads, freguesias, left = FALSE)
roads_by_freguesia$length_segment <- st_length(roads_by_freguesia)
road_length_by_freguesia <- roads_by_freguesia |>
  group_by(freguesia) |>
  summarise(road_length = sum(length_segment))
road_length_by_freguesia <- road_length_by_freguesia |>
  st_drop_geometry()


# Pedestrians ----
  # Get OSM pedestrian infrastructure data for AML
osm_pedpaths <- opq(bbox = municipios |> sf::st_bbox()) |>
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


# Bicycles ----
  # Get OSM cycleway data for AML
osm_cycleways <- opq(bbox = municipios |> sf::st_bbox()) |>
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
  select(osm_id, name, highway, geometry)
#mapview(aml_cycleways)

  # Disaggregate and measure cycleway length by Freguesia
cycleways_by_freguesia <- st_join(aml_cycleways, freguesias, left = FALSE)
cycleways_by_freguesia$length_segment <- st_length(cycleways_by_freguesia)
cycleway_length_by_freguesia <- cycleways_by_freguesia |>
  group_by(freguesia) |>
  summarise(cycleway_length = sum(length_segment))
cycleway_length_by_freguesia <- cycleway_length_by_freguesia |>
  st_drop_geometry()


  # Evaluate cycleway quality by freguesia
segregated_cycleways <- cycle_net_pt |> filter(cycle_segregation == "Cycle track or lane")
segregated_by_freguesia <- st_join(segregated_cycleways, freguesias, left = FALSE)
segregated_by_freguesia$length_segment <- st_length(segregated_by_freguesia)
segregated_length_by_freguesia <- segregated_by_freguesia |>
  group_by(freguesia) |>
  summarise(segregated_cycleway_length = sum(length_segment))
segregated_length_by_freguesia <- segregated_length_by_freguesia |>
  st_drop_geometry()
  
  

# Compute ratio of pedpath/cycleway to roads ----
freguesias_by_infrastructure <- freguesias |>
  left_join(road_length_by_freguesia, by = "freguesia") |>
  left_join(pedpath_length_by_freguesia, by = "freguesia") |>
  left_join(cycleway_length_by_freguesia, by = "freguesia") |>
  left_join(segregated_length_by_freguesia, by = "freguesia") |>
  mutate(
    road_length = ifelse(is.na(road_length), 0, road_length),
    pedpath_length = ifelse(is.na(pedpath_length), 0, pedpath_length),
    cycleway_length = ifelse(is.na(cycleway_length), 0, cycleway_length),
    segregated_cycleway_length = ifelse(is.na(segregated_cycleway_length), 0, segregated_cycleway_length),
    pedpath_to_road_ratio = ifelse(road_length > 0, pedpath_length / road_length, 0),
    cycleway_to_road_ratio = ifelse(road_length > 0, cycleway_length / road_length, 0),
    cycling_quality_ratio = ifelse(cycleway_length > 0, segregated_cycleway_length / cycleway_length, 0)
  )
mapview(freguesias_by_infrastructure, zcol = "pedpath_to_road_ratio")
mapview(freguesias_by_infrastructure, zcol = "cycleway_to_road_ratio")
mapview(freguesias_by_infrastructure, zcol = "cycling_quality_ratio")


# Attempt to measure network continuity ----
  # Bicycles
aml_cycling_net_graph <- as_sfnetwork(cycleways_by_freguesia, directed = FALSE)
cycling_edges <- aml_cycling_net_graph |> activate("edges") |> st_as_sf() |> summarise(n = n()) |> pull(n)
cycling_nodes <- aml_cycling_net_graph |> activate("nodes") |> st_as_sf() |> summarise(n = n()) |> pull(n)
cycling_components <-  aml_cycling_net_graph |>
  activate("nodes") |>
  mutate(component = group_components()) |>
  pull(component) |>
  unique() |>
  length()
cycling_R <- cycling_edges - cycling_nodes + cycling_components

plot(aml_cycling_net_graph, 
     main = "AML Cycling Network",
     col = "blue", 
     lwd = 0.5)

    # Create a graph for each freguesia
freguesias2 <- freguesias$freguesia
freguesia_cycling_net_graph <- list()
for(i in freguesias2) {
  # Filter edges that belong to the current parish
  edges_freguesia <- cycleways_by_freguesia[cycleways_by_freguesia$freguesia == i, ]
  if(nrow(edges_freguesia) > 0) {  # Check if there are edges
    freguesia_cycling_net_graph[[i]] <- as_sfnetwork(edges_freguesia, directed = FALSE)
  } 
  else {
    freguesia_cycling_net_graph[[i]] <- list(0)  # No edges for this parish
  }
}



# Save results ----
saveRDS(freguesias_by_infrastructure |> st_drop_geometry(), "/data/IMPT/mobility/freguesias_infrastructure_ratio.rds")
saveRDS(aml_cycling_net_graph, "/data/IMPT/mobility/aml_cycling_net_graph.rds")
saveRDS(freguesia_cycling_net_graph, "/data/IMPT/mobility/freguesia_cycling_net_graph.rds")
