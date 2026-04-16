#testing isochrones for PT stops


options(openrouteservice.url = "https://server.ushift.pt/ors/")
# Set credentials
Sys.setenv(ORS_API_KEY = "AMwi5LU1NCe1ERyQuUHTe2pulOFKcdq0")
# Sys.unsetenv("ORS_API_KEY")
# Validate
getOption("openrouteservice.url")
#> [1] "https://server.ushift.pt/ors/"
Sys.getenv("ORS_API_KEY")
#> [1] "AMwi5LU1NCe1ERyQuUHTe2pulOFKcdq0"

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

# Create service area buffers for each type of PT stops
train_ferry_stations = bind_rows(all_stops$`/data/IMPT/gtfs/processed/gtfs_comboios_de_portugal.zip`,all_stops$`/data/IMPT/gtfs/processed/gtfs_fertagus.zip`,all_stops$`/data/IMPT/gtfs/processed/gtfs_transtejo_soflusa.zip`) |> st_as_sf() |> st_filter(limit_bbox)
train_ferry_stations <- train_ferry_stations |> select(stop_id, stop_code, stop_name, geometry) |> st_transform(4326)
train_ferry_coordinates <- st_coordinates(train_ferry_stations) |> as.data.frame() |> select(X, Y) |> rename(lon=X, lat=Y) |> mutate(id=train_ferry_stations$stop_id) |> select(id, lon, lat)
# stop_service_areas <- ors_isochrones(
#   locations = train_ferry_coordinates,
#   profile = "foot-walking",
#   range = 15,
#   range_type = "time"
# )
metro_stations = bind_rows(all_stops$`/data/IMPT/gtfs/processed/gtfs_metropolitano_de_lisboa.zip`,all_stops$`/data/IMPT/gtfs/processed/gtfs_metro_transportes_do_sul.zip`) |> st_as_sf() |> st_filter(limit_bbox)
metro_stations <- metro_stations |> select(stop_id, stop_code, stop_name, geometry) |> st_transform(4326)
metro_coordinates <- st_coordinates(metro_stations) |> as.data.frame() |> select(X, Y) |> rename(lon=X, lat=Y)
# stop_service_areas <- ors_isochrones(
#   locations = metro_coordinates,
#   profile = "foot-walking",
#   range = 5,
#   range_type = "time"
# )
bus_stations = bind_rows(all_stops$`/data/IMPT/gtfs/processed/gtfs_carris_metropolitana.zip`,all_stops$`/data/IMPT/gtfs/processed/gtfs_carris_municipal.zip`,all_stops$`/data/IMPT/gtfs/processed/gtfs_mobicascais.zip`,all_stops$`/data/IMPT/gtfs/processed/gtfs_transportes_colectivos_do_barreiro.zip`) |> st_as_sf() |> st_filter(limit_bbox)
bus_stations <- bus_stations |> select(stop_id, stop_code, stop_name, geometry) |> st_transform(4326)
bus_coordinates <- st_coordinates(bus_stations) |> as.data.frame() |> select(X, Y) |> rename(lon=X, lat=Y)
# stop_service_areas <- ors_isochrones(
#   locations = bus_coordinates,
#   profile = "foot-walking",
#   range = 5,
#   range_type = "time"
# )

### Get isochrones for train/ferry stations using r5r (PRELIMINARY - NOT COMPLETE)
# system.file returns the directory with example data inside the r5r package
# set data path to directory containing your own data if not running this example
r5r_network <- r5r::build_network("/data/IMPT/geo/r5r/")
# data_path <- "/data/IMPT/geo/r5r"
# r5r_network <- build_network(data_path)
# isochrone intervals
time_interval <- 15
# routing inputs
mode <- "WALK"
departure_datetime <- as.POSIXct("13-05-2019 14:00:00", format = "%d-%m-%Y %H:%M:%S")
# calculate travel time matrix
iso_train_ferry <- r5r::isochrone(
  r5r_network,
  origins = train_ferry_coordinates,
  mode = mode,
  polygon_output = TRUE, 
  cutoffs = time_interval,
  departure_datetime = departure_datetime,
  progress = TRUE,
  zoom = 10
)

