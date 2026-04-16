# Compute mobility parameters for every mode
# Purpose     Determine availability/coverage of PT, shared mobility, existence of infrastructure (pedestrian and cycling)
# Scale       hex, parish, municipality
# Issues      Service area buffers not working yet (ORS issue)


library(tidytransit)
library(mapview)
library(osmdata)
library(sf)
library(igraph)
library(tidygraph)
library(tidytransit)
library(sfnetworks)
library(dplyr)
library(openrouteservice)


# Availability/coverage of public transportation -----------------------------------------------------------------------
options(openrouteservice.url = "https://server.ushift.pt/ors/v2/")
openrouteservice::ors_api_key("AMwi5LU1NCe1ERyQuUHTe2pulOFKcdq0=")


# Start by getting all PT stops in AML
# Attention! Must be run at ushift@alfa, or addapted to use local (make sure to upload to server after)
gtfs_paths <- list.files("/data/gtfs/processed", pattern = "\\.zip$", full.names = TRUE)
all_stops <- list()
for (i in gtfs_paths) {
  gtfs_original <- tidytransit::read_gtfs(i)
  message("Running for ", gtfs_original$agency$agency_name[1])
  gtfs <- tidytransit::filter_feed_by_date(gtfs_original, "2026-02-04")
  summary(gtfs)
  stops_sf <- tidytransit::stops_as_sf(gtfs$stops)
  all_stops[[i]] <- stops_sf
}
# mapview(all_stops, legend=FALSE)

# Create euclidean buffers for all PT stops
all_stops_combined <- bind_rows(all_stops) |>
  st_as_sf() |>
  st_filter(limit_bbox)
# mapview(all_stops_combined, legend=FALSE)
all_stops_combined <- all_stops_combined |> select(stop_id, stop_code, stop_name, geometry)
stops_buffers <- st_buffer(
  # Convert to 3857 crs
  all_stops_combined |> st_transform(3857),
  dist = 500
) # 500m euclidean buffer around stops

# # Create service area buffers for all PT stops
# all_stops_combined_2 <- all_stops_combined |> st_transform(4326)
# stop_coordinates <- st_coordinates(all_stops_combined_2) |> as.data.frame() |> select(X, Y) |> rename(lon=X, lat=Y)
# stop_service_areas <- ors_isochrones(
#   locations = stop_coordinates,
#   profile = "foot-walking",
#   range = 500,
#   range_type = "distance"
# )
# stops_service_buffers_500m <- st_buffer(
#   # Convert to 3857 crs
#   all_stops_combined |> st_transform(3857),
#   dist = 500
# ) #500m service area buffer around stops

# Evaluate population coverage by PT stop
# mapview(census, zcol="N_INDIVIDUOS")
census_stops <- census |>
  select(id, N_INDIVIDUOS, SHAPE_Length, SHAPE_Area, dicofre24, freguesia, municipio, geom) |>
  st_transform(3857) |>
  filter(!is.na(freguesia))
census_stops$reachable_stops <- lengths(st_intersects(census_stops, stops_buffers))
census_stops$has_stops <- census_stops$reachable_stops > 0
census_stops$pop_near_stops <- census_stops$N_INDIVIDUOS * census_stops$has_stops
freguesias_by_stops <- census_stops |>
  st_drop_geometry() |> # Drop geometry for aggregation
  group_by(freguesia) |>
  summarise(
    total_points = n(),
    served_population = sum(pop_near_stops),
    freguesia_population = sum(N_INDIVIDUOS),
    ratio_served_population = round(served_population / freguesia_population, digits = 2)
  )
municipios_by_stops <- census_stops |>
  st_drop_geometry() |> # Drop geometry for aggregation
  group_by(municipio) |>
  summarise(
    total_points = n(),
    served_population = sum(pop_near_stops),
    municipio_population = sum(N_INDIVIDUOS),
    ratio_served_population = round(served_population / municipio_population, digits = 2)
  )
census_stops_grid <- census_stops |> st_join(grid |> select(id) |> rename(grid_id = id) |> st_transform(3857), join = st_within)
grid_by_stops <- census_stops_grid |>
  st_drop_geometry() |> # Drop geometry for aggregation
  group_by(grid_id) |>
  summarise(
    total_points = n(),
    served_population = sum(pop_near_stops),
    grid_population = sum(N_INDIVIDUOS),
    ratio_served_population = round(served_population / grid_population, digits = 2)
  )
# mapview(grid |> left_join(grid_by_stops, by = c("id" = "grid_id")), zcol = "ratio_served_population")

impt_write(freguesias_by_stops, "/mobility/freguesias_stops_coverage.rds")
impt_write(municipios_by_stops, "/mobility/municipios_stops_coverage.rds")
impt_write(grid_by_stops, "/mobility/grid_stops_coverage.rds")
impt_write(freguesias_by_stops, "/mobility/freguesias_stops_coverage.csv")
impt_write(municipios_by_stops, "/mobility/municipios_stops_coverage.csv")
impt_write(grid_by_stops, "/mobility/grid_stops_coverage.csv")


# Shared Mobility Availability --------------------------------------------

# Grid from 00_data_load.R
grid_shared_mob <- grid
freguesias_shared_mob <- freguesias
municipios_shared_mob <- municipios
aml_shared_mobility <- impt_read("/BaseDados_PMMUS/11-ModosPartilhados/11 - ModosPartilhados.gpkg", layer = "Pontos-partilha_amL") |>
  st_transform(st_crs(grid_shared_mob))
# mapview(aml_shared_mobility)
# Get number of shared mobility locations for different scales
grid_shared_mob$shared_mobility_points <- lengths(st_intersects(grid_shared_mob, aml_shared_mobility))
freguesias_shared_mob$shared_mobility_points <- lengths(st_intersects(freguesias_shared_mob, aml_shared_mobility))
municipios_shared_mob$shared_mobility_points <- lengths(st_intersects(municipios_shared_mob, aml_shared_mobility))

# impt_write(grid_shared_mob, "/mobility/grid_shared_mobility.gpkg")
# impt_write(municipios_shared_mob, "/mobility/municipios_shared_mobility.gpkg")
# impt_write(freguesias_shared_mob, "/mobility/freguesias_shared_mobility.gpkg")
impt_write(freguesias_shared_mob, "mobility/freguesias_shared_mobility.rds")
impt_write(municipios_shared_mob, "mobility/municipios_shared_mobility.rds")
impt_write(grid_shared_mob, "mobility/grid_shared_mobility.rds")

impt_write(freguesias_shared_mob |> select(dtmnfr, shared_mobility_points) |> st_drop_geometry(), "mobility/freguesias_shared_mobility.csv")
impt_write(municipios_shared_mob |> select(municipio, shared_mobility_points) |> st_drop_geometry(), "mobility/municipios_shared_mobility.csv")
impt_write(grid_shared_mob |> select(id, shared_mobility_points) |> st_drop_geometry(), "mobility/grid_shared_mobility.csv")
# mapview(grid |> left_join(grid_shared_mob |> st_drop_geometry() |> select(id, shared_mobility_points), by = c("id" = "id")), zcol = "shared_mobility_points")
# mapview(freguesias |> left_join(freguesias_shared_mob |> st_drop_geometry() |> select(dtmnfr, shared_mobility_points), by = c("dtmnfr" = "dtmnfr")), zcol = "shared_mobility_points")
# mapview(municipios |> left_join(municipios_shared_mob |> st_drop_geometry() |> select(municipio, shared_mobility_points), by = c("municipio" = "municipio")), zcol = "shared_mobility_points")


# Existence of infrastructure ---------------------------------------------

### Roads ----
# Get all road infrastructure OSM data for AML
# osm_roads <- opq(bbox = municipios |> sf::st_bbox()) |>
#   # Highways with tags "service", "track" and "road" are excluded
#   add_osm_feature(key = "highway", value = c(
#     "motorway", "trunk", "primary", "secondary",
#     "tertiary", "unclassified", "residential", "motorway_link", "trunk_link",
#     "primary_link", "secondary_link", "tertiary_link", "living_street"
#   )) |>
#   osmdata_sf()
# aml_roads <- osm_roads$osm_lines |> st_as_sf()
osm_roads = st_read("/data/IMPT/geo/r5r/IMPT_Road_network.osm.pbf", layer="lines") # from hot export tool (osm pacakage has timeouts)
# Remove unnecessary columns
aml_roads <- osm_roads |> select(osm_id, name, highway, geometry)
# mapview(aml_roads)

# Disaggregate and measure road length for different scales
roads_by_freguesia <- st_join(aml_roads, freguesias, left = FALSE)
roads_by_freguesia$length_segment <- st_length(roads_by_freguesia |> st_transform(3857))
road_length_by_freguesia <- roads_by_freguesia |>
  group_by(freguesia) |>
  summarise(road_length = sum(length_segment))
road_length_by_freguesia <- road_length_by_freguesia |>
  st_drop_geometry()

roads_by_municipio <- st_join(aml_roads, municipios, left = FALSE)
roads_by_municipio$length_segment <- st_length(roads_by_municipio |> st_transform(3857))
road_length_by_municipio <- roads_by_municipio |>
  group_by(municipio) |>
  summarise(road_length = sum(length_segment))
road_length_by_municipio <- road_length_by_municipio |>
  st_drop_geometry()

roads_by_grid <- st_join(aml_roads, grid |> select(id) |> rename(grid_id = id), join = st_within)
roads_by_grid$length_segment <- st_length(roads_by_grid |> st_transform(3857))
road_length_by_grid <- roads_by_grid |>
  group_by(grid_id) |>
  summarise(road_length = sum(length_segment))
road_length_by_grid <- road_length_by_grid |> st_drop_geometry()
# mapview(grid |> left_join(road_length_by_grid, by = c("id" = "grid_id")), zcol = "road_length")
# mapview(freguesias |> left_join(road_length_by_freguesia, by = "freguesia"), zcol = "road_length")
# mapview(municipios |> left_join(road_length_by_municipio, by = "municipio"), zcol = "road_length")

### Pedestrians ----
# Get OSM pedestrian infrastructure data for AML
# osm_pedpaths <- opq(bbox = municipios |> sf::st_bbox()) |>
#   add_osm_features(list(
#     # Residential streets are included due to them not having
#     # path or sidewalk tags in OSM, but being mostly walkable.
#     # Separate footpaths
#     "highway" = c("footway", "residential", "pedestrian", "steps"),
#     "footway" = c("sidewalk", "crossing", "path", "platform", "corridor", "alley", "track"),
#     # Roads with sidewalk tags
#     "sidewalk" = c("both", "left", "right")
#   )) |>
#   osmdata_sf()
# osm_pedpaths = osm_pedpaths$osm_lines |> st_as_sf()
# impt_write(osm_pedpaths, "/mobility/osm_pedpaths.Rds")
osm_pedpaths = impt_read("/mobility/osm_pedpaths.Rds")
# Remove unnecessary columns
aml_pedpaths <- osm_pedpaths |> select(osm_id, name, highway, sidewalk, geometry)
# mapview(aml_pedpaths)

# Disaggregate and measure pedpath length by Freguesia
pedpaths_by_freguesia <- st_join(aml_pedpaths, freguesias, left = FALSE)
pedpaths_by_freguesia$length_segment <- st_length(pedpaths_by_freguesia |> st_transform(3857))
pedpath_length_by_freguesia <- pedpaths_by_freguesia |>
  group_by(freguesia) |>
  summarise(pedpath_length = sum(length_segment))
pedpath_length_by_freguesia <- pedpath_length_by_freguesia |>
  st_drop_geometry()

pedpaths_by_municipio <- st_join(aml_pedpaths, municipios, left = FALSE)
pedpaths_by_municipio$length_segment <- st_length(pedpaths_by_municipio |> st_transform(3857))
pedpath_length_by_municipio <- pedpaths_by_municipio |>
  group_by(municipio) |>
  summarise(pedpath_length = sum(length_segment))
pedpath_length_by_municipio <- pedpath_length_by_municipio |> st_drop_geometry()

pedpaths_by_grid <- st_join(aml_pedpaths, grid |> select(id) |> rename(grid_id = id), join = st_within)
pedpaths_by_grid$length_segment <- st_length(pedpaths_by_grid |> st_transform(3857))
pedpath_length_by_grid <- pedpaths_by_grid |>
  group_by(grid_id) |>
  summarise(pedpath_length = sum(length_segment))
pedpath_length_by_grid <- pedpath_length_by_grid |> st_drop_geometry()

# mapview(grid |> left_join(pedpath_length_by_grid, by = c("id" = "grid_id")), zcol = "pedpath_length")
# mapview(freguesias |> left_join(pedpath_length_by_freguesia, by = "freguesia"), zcol = "pedpath_length")
# mapview(municipios |> left_join(pedpath_length_by_municipio, by = "municipio"), zcol = "pedpath_length")

### Bicycles ----
# Get OSM cycleway data for AML
# see test-code/bike_infrastructuture_quality.R

all_cycleways <- impt_read("mobility/cycle_network_class.gpkg")
# mapview(all_cycleways)
segregated_cycleways <- all_cycleways |> filter(cycle_segregation == "Cycle track or lane")

cycleways_by_freguesia <- st_join(all_cycleways, freguesias, left = FALSE)
cycleways_by_freguesia$length_segment <- st_length(cycleways_by_freguesia |> st_transform(3857))
cycleway_length_by_freguesia <- cycleways_by_freguesia |>
  group_by(freguesia) |>
  summarise(cycleway_length = sum(length_segment))
cycleway_length_by_freguesia <- cycleway_length_by_freguesia |>
  st_drop_geometry()

segregated_by_freguesia <- st_join(segregated_cycleways, freguesias, left = FALSE)
segregated_by_freguesia$length_segment <- st_length(segregated_by_freguesia |> st_transform(3857))
segregated_length_by_freguesia <- segregated_by_freguesia |>
  group_by(freguesia) |>
  summarise(segregated_cycleway_length = sum(length_segment))
segregated_length_by_freguesia <- segregated_length_by_freguesia |>
  st_drop_geometry()

cycleways_by_municipio <- st_join(all_cycleways, municipios, left = FALSE)
cycleways_by_municipio$length_segment <- st_length(cycleways_by_municipio |> st_transform(3857))
cycleway_length_by_municipio <- cycleways_by_municipio |>
  group_by(municipio) |>
  summarise(cycleway_length = sum(length_segment))
cycleway_length_by_municipio <- cycleway_length_by_municipio |> st_drop_geometry()

segregated_by_municipio <- st_join(segregated_cycleways, municipios, left = FALSE)
segregated_by_municipio$length_segment <- st_length(segregated_by_municipio |> st_transform(3857))
segregated_length_by_municipio <- segregated_by_municipio |>
  group_by(municipio) |>
  summarise(segregated_cycleway_length = sum(length_segment))
segregated_length_by_municipio <- segregated_length_by_municipio |> st_drop_geometry()

cycleways_by_grid <- st_join(all_cycleways, grid |> select(id) |> rename(grid_id = id), join = st_within)
cycleways_by_grid$length_segment <- st_length(cycleways_by_grid |> st_transform(3857))
cycleway_length_by_grid <- cycleways_by_grid |>
  group_by(grid_id) |>
  summarise(cycleway_length = sum(length_segment))
cycleway_length_by_grid <- cycleway_length_by_grid |> st_drop_geometry()

segregated_by_grid <- st_join(segregated_cycleways, grid |> select(id) |> rename(grid_id = id), join = st_within)
segregated_by_grid$length_segment <- st_length(segregated_by_grid |> st_transform(3857))
segregated_length_by_grid <- segregated_by_grid |>
  group_by(grid_id) |>
  summarise(segregated_cycleway_length = sum(length_segment))
segregated_length_by_grid <- segregated_length_by_grid |> st_drop_geometry()

# mapview(grid |> left_join(cycleway_length_by_grid, by = c("id" = "grid_id")), zcol = "cycleway_length")
# mapview(grid |> left_join(segregated_length_by_grid, by = c("id" = "grid_id")), zcol = "segregated_cycleway_length")
# mapview(freguesias |> left_join(cycleway_length_by_freguesia, by = "freguesia"), zcol = "cycleway_length")
# mapview(freguesias |> left_join(segregated_length_by_freguesia, by = "freguesia"), zcol = "segregated_cycleway_length")
# mapview(municipios |> left_join(cycleway_length_by_municipio, by = "municipio"), zcol = "cycleway_length")
# mapview(municipios |> left_join(segregated_length_by_municipio, by = "municipio"), zcol = "segregated_cycleway_length")


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
  ) |>
  mutate(across(where(is.numeric), ~ round(., 2))) # Round all numeric values to 2 decimal places
# mapview(freguesias_by_infrastructure, zcol = "pedpath_to_road_ratio")
# mapview(freguesias_by_infrastructure, zcol = "cycleway_to_road_ratio")
# mapview(freguesias_by_infrastructure, zcol = "cycling_quality_ratio")

municipios_by_infrastructure <- municipios |>
  left_join(road_length_by_municipio, by = "municipio") |>
  left_join(pedpath_length_by_municipio, by = "municipio") |>
  left_join(cycleway_length_by_municipio, by = "municipio") |>
  left_join(segregated_length_by_municipio, by = "municipio") |>
  mutate(
    road_length = ifelse(is.na(road_length), 0, road_length),
    pedpath_length = ifelse(is.na(pedpath_length), 0, pedpath_length),
    cycleway_length = ifelse(is.na(cycleway_length), 0, cycleway_length),
    segregated_cycleway_length = ifelse(is.na(segregated_cycleway_length), 0, segregated_cycleway_length),
    pedpath_to_road_ratio = ifelse(road_length > 0, pedpath_length / road_length, 0),
    cycleway_to_road_ratio = ifelse(road_length > 0, cycleway_length / road_length, 0),
    cycling_quality_ratio = ifelse(cycleway_length > 0, segregated_cycleway_length / cycleway_length, 0)
  ) |>
  mutate(across(where(is.numeric), ~ round(., 2))) # Round all numeric values to 2 decimal places
# mapview(municipios_by_infrastructure, zcol = "pedpath_to_road_ratio")
# mapview(municipios_by_infrastructure, zcol = "cycleway_to_road_ratio")
# mapview(municipios_by_infrastructure, zcol = "cycling_quality_ratio")

grid_by_infrastructure <- grid |>
  left_join(road_length_by_grid, by = c("id" = "grid_id")) |>
  left_join(pedpath_length_by_grid, by = c("id" = "grid_id")) |>
  left_join(cycleway_length_by_grid, by = c("id" = "grid_id")) |>
  left_join(segregated_length_by_grid, by = c("id" = "grid_id")) |>
  mutate(
    road_length = ifelse(is.na(road_length), 0, road_length),
    pedpath_length = ifelse(is.na(pedpath_length), 0, pedpath_length),
    cycleway_length = ifelse(is.na(cycleway_length), 0, cycleway_length),
    segregated_cycleway_length = ifelse(is.na(segregated_cycleway_length), 0, segregated_cycleway_length),
    pedpath_to_road_ratio = ifelse(road_length > 0, pedpath_length / road_length, 0),
    cycleway_to_road_ratio = ifelse(road_length > 0, cycleway_length / road_length, 0),
    cycling_quality_ratio = ifelse(cycleway_length > 0, segregated_cycleway_length / cycleway_length, 0)
  ) |>
  mutate(across(where(is.numeric), ~ round(., 2))) # Round all numeric values to 2 decimal places
# mapview(grid_by_infrastructure, zcol = "pedpath_to_road_ratio")
# mapview(grid_by_infrastructure, zcol = "cycleway_to_road_ratio")
# mapview(grid_by_infrastructure, zcol = "cycling_quality_ratio")


### Save results ----
# impt_write(freguesias_by_infrastructure, "/mobility/freguesias_infrastructure_ratio.gpkg")
# impt_write(freguesias_by_infrastructure |> st_drop_geometry(), "/mobility/freguesias_infrastructure_ratio.rds")

impt_write(freguesias_by_infrastructure |> st_drop_geometry(), "/mobility/freguesias_infrastructure_ratio.csv")
impt_write(municipios_by_infrastructure |> st_drop_geometry(), "/mobility/municipios_infrastructure_ratio.csv")
impt_write(grid_by_infrastructure |> st_drop_geometry(), "/mobility/grid_infrastructure_ratio.csv")
