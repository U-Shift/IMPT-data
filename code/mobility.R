# Objective: Compute mobility parameters for every mode
# Done so far:
  # Car: Ownership rates (in veh_ownership.R)
  # Walking: Existence of infrastructure.
  # Cycling: Existence of infrastructure, Quality of infrastructure.
# Almost complete:
  # PT: Availability/coverage, Shared mobility availability
# Not started:
  # PT: Night/weekend service.

library(gtfstools)
library(mapview)
library(osmdata)
library(sf)
library(igraph)
library(tidygraph)
library(tidytransit)
library(sfnetworks)

### Availability/coverage of public transportation ----

  # Start by getting all PT stops in AML
gtfs_paths <- list.files(IMPT_URL("/gtfs/processed"), pattern="\\.zip$" , full.names = TRUE)
all_stops <- list()
for (i in gtfs_paths) {
  gtfs <- read_gtfs(i)
  stops_sf <- stops_as_sf(gtfs$stops)
  all_stops[[i]] <- stops_sf
}
#mapview(all_stops)

  # Create 250m buffers for all PT stops
all_stops_combined = bind_rows(all_stops) |> st_as_sf()
all_stops_combined <- all_stops_combined |> select(stop_id, stop_code, stop_name, geometry)
stops_buffers <- st_buffer(all_stops_combined, dist = 250) #250m buffer around stops

  # Evaluate population coverage by PT stop
census_stops <- census |> select(id, N_INDIVIDUOS, SHAPE_Length, SHAPE_Area, dicofre24, freguesia, municipio, geom)
census_stops$reachable_stops <- lengths(st_intersects(census_stops, stops_buffers))
census_stops$has_stops <- census_stops$reachable_stops>0
census_stops$pop_near_stops <- census_stops$N_INDIVIDUOS * census_stops$has_stops
freguesias_by_stops <- census_stops |>
  st_drop_geometry() |>  # Drop geometry for aggregation
  group_by(freguesia) |>
  summarise(
    total_points = n(),
    served_population = sum(pop_near_stops),
    freguesia_population = sum(N_INDIVIDUOS),
    ratio_served_population = (served_population / freguesia_population)
  )

### Shared Mobility Availability ----
  # Grid from data_load.R
grid_shared_mob = grid
freguesias_shared_mob = freguesias
aml_shared_mobility = st_read(IMPT_URL("/BaseDados_PMMUS/11-ModosPartilhados/11 - ModosPartilhados.gpkg"), layer = "Pontos-partilha_amL") |>
  st_transform(st_crs(grid_shared_mob))
  # Get number of shared mobility locations by grid hexagon and freguesia
grid_shared_mob$shared_mobility_points <- lengths(st_intersects(grid_shared_mob, aml_shared_mobility))
freguesias_shared_mob$shared_mobility_points <- lengths(st_intersects(freguesias_shared_mob, aml_shared_mobility))



### Roads ----
  # Get all road infrastructure OSM data for AML
osm_roads <- opq(bbox = municipios |> sf::st_bbox()) |>
  # Highways with tags "service", "track" and "road" are excluded
  add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", 
                  "tertiary", "unclassified", "residential", "motorway_link", "trunk_link",
                  "primary_link", "secondary_link", "tertiary_link", "living_street")
  ) |>
  osmdata_sf()
aml_roads <- osm_roads$osm_lines |> st_as_sf()
  # Remove unnecessary columns
aml_roads <- aml_roads |> select(osm_id, name, highway, geometry)
#mapview(aml_roads)

# Disaggregate and measure road length by Freguesia
roads_by_freguesia <- st_join(aml_roads, freguesias, left = FALSE)
roads_by_freguesia$length_segment <- st_length(roads_by_freguesia |> st_transform(3857))
road_length_by_freguesia <- roads_by_freguesia |>
  group_by(freguesia) |>
  summarise(road_length = sum(length_segment))
road_length_by_freguesia <- road_length_by_freguesia |>
  st_drop_geometry()


### Pedestrians ----
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
aml_pedpaths <- osm_pedpaths$osm_lines |> st_as_sf()
  # Remove unnecessary columns
aml_pedpaths <- aml_pedpaths |> select(osm_id, name, highway, sidewalk, geometry)
#mapview(aml_pedpaths)

# Disaggregate and measure pedpath length by Freguesia
pedpaths_by_freguesia <- st_join(aml_pedpaths, freguesias, left = FALSE)
pedpaths_by_freguesia$length_segment <- st_length(pedpaths_by_freguesia |> st_transform(3857))
pedpath_length_by_freguesia <- pedpaths_by_freguesia |>
  group_by(freguesia) |>
  summarise(pedpath_length = sum(length_segment))
pedpath_length_by_freguesia <- pedpath_length_by_freguesia |>
  st_drop_geometry()


### Bicycles ----
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
aml_cycleways <- osm_cycleways$osm_lines |> st_as_sf()
    # Remove unnecessary columns
aml_cycleways <- aml_cycleways |> select(osm_id, name, highway, geometry)
#mapview(aml_cycleways)

  # Disaggregate and measure cycleway length by Freguesia
cycleways_by_freguesia <- st_join(aml_cycleways, freguesias, left = FALSE)
cycleways_by_freguesia$length_segment <- st_length(cycleways_by_freguesia |> st_transform(3857)) 
cycleway_length_by_freguesia <- cycleways_by_freguesia |>
  group_by(freguesia) |>
  summarise(cycleway_length = sum(length_segment))
cycleway_length_by_freguesia <- cycleway_length_by_freguesia |>
  st_drop_geometry()


  # Evaluate cycleway quality by freguesia
segregated_cycleways <- st_read("/data/IMPT/mobility/cycle_network_class.gpkg") |> filter(cycle_segregation == "Cycle track or lane")
segregated_by_freguesia <- st_join(segregated_cycleways, freguesias, left = FALSE)
segregated_by_freguesia$length_segment <- st_length(segregated_by_freguesia)
segregated_length_by_freguesia <- segregated_by_freguesia |>
  group_by(freguesia) |>
  summarise(segregated_cycleway_length = sum(length_segment))
segregated_length_by_freguesia <- segregated_length_by_freguesia |>
  st_drop_geometry()
  
  

### Compute ratio of pedpath/cycleway to roads ----
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



# Save results ----
st_write(freguesias_by_infrastructure, "/data/IMPT/mobility/freguesias_infrastructure_ratio.gpkg", delete_dsn = TRUE)
st_write(grid_shared_mob, "/data/IMPT/mobility/grid_shared_mobility.gpkg", delete_dsn = TRUE)
saveRDS(freguesias_shared_mob, "/data/IMPT/mobility/freguesias_shared_mobility.rds")
saveRDS(freguesias_by_infrastructure |> st_drop_geometry(), "/data/IMPT/mobility/freguesias_infrastructure_ratio.rds")
saveRDS(freguesias_by_stops |> st_drop_geometry(), "/data/IMPT/mobility/freguesias_stops_coverage.rds")
