# Waiting times and frequencies ---------------------------------------------------------------------
pois_transit_headways = st_read(IMPT_URL("/mobility_transit/transit_stops_headways.gpkg"))
summary(pois_transit_headways)

# For each stop, calculate the number of residents that it serves, considering a buffer of 250 meters and the residents at census$N_INDIVIDUOS
stops_buffers = st_buffer(pois_transit_headways |> st_transform(crs=3857), dist = 500) #500m buffer around stops
census_population = census |> select(id, N_INDIVIDUOS, geom) |> st_transform(crs=3857)
stop_population = st_intersection(census_population, stops_buffers) |> 
  st_drop_geometry() |>
  group_by(stop_id) |> 
  summarise(population_served = sum(N_INDIVIDUOS))

pois_transit_headways_population = pois_transit_headways |>
  left_join(stop_population, by = "stop_id")

mapview(pois_transit_headways_population, zcol="population_served", layer.name="Population served within 500 buffer")
# mapview(census, col.regions="#ffb3b3") + mapview(pois_transit_headways_population, zcol="population_served", layer.name="Population served within 500 buffer")
# mapview(census, col.regions="#ffb3b3") + mapview(stops_buffers, layer.name="750 m buffer around stops")

pois_transit_headways_population = pois_transit_headways_population |> 
  filter(!is.na(population_served) & !is.na(frequency_peak))

# Aggregate headway (average) by grid, parish and county, weighted by population served
aggregated_transit_times_for_geometry = function(grid) {
  return (
    grid |> 
      summarise(
        # Frequencies and headways, weighted
        weighted_headway_peak = round(weighted.mean(headway_peak, population_served, na.rm=TRUE), digits=2),
        weighted_headway_day = round(weighted.mean(headway_day, population_served, na.rm=TRUE), digits=2),
        weighted_frequency_peak = round(weighted.mean(frequency_peak, population_served, na.rm=TRUE), digits=2),
        weighted_frequency_day = round(weighted.mean(frequency_day, population_served, na.rm=TRUE), digits=2),
        # Total values
        total_population_served = sum(population_served, na.rm=TRUE),
        total_frequency_peak = sum(frequency_peak, na.rm=TRUE),
        total_frequency_day = sum(frequency_day, na.rm=TRUE),
        n_stops = n()
      ) |> 
      mutate(
        # Convert headway, in seconds, in waiting time
        weighted_waiting_time_peak = round(weighted_headway_peak / 2 / 60, digits=2), # in minutes
        weighted_waiting_time_day = round(weighted_headway_day / 2 / 60, digits=2) # in minutes
      )
  )
}
grid_headways = aggregated_transit_times_for_geometry(
  grid |> st_join(pois_transit_headways_population, join = st_intersects) |> 
  st_drop_geometry() |>
  group_by(id)
)
  
grid_headways_sf = grid |> left_join(grid_headways, by = "id")
mapview(grid_headways_sf, zcol="weighted_frequency_peak")
mapview(grid_headways_sf, zcol="weighted_waiting_time_peak")
mapview(grid_headways_sf, zcol="weighted_waiting_time_peak") + mapview(pois_transit_headways_population)

st_write(grid_headways_sf, IMPT_URL("/mobility_transit/grid_headways.gpkg"), delete_dsn = TRUE)
st_write(grid_headways, IMPT_URL("/mobility_transit/grid_headways.csv"))

freguesia_headways = aggregated_transit_times_for_geometry(
  freguesias |> st_join(pois_transit_headways_population, join = st_intersects) |> 
    st_drop_geometry() |>
    group_by(dtmnfr)
)
freguesia_headways_sf = freguesias |> left_join(freguesia_headways, by = c("dtmnfr"))
mapview(freguesia_headways_sf, zcol="weighted_frequency_peak")
mapview(freguesia_headways_sf, zcol="weighted_waiting_time_peak")
mapview(freguesia_headways_sf, zcol="weighted_waiting_time_day")
# mapview(freguesia_headways_sf, zcol="weighted_frequency_peak") + mapview(pois_transit_headways_population)

st_write(freguesia_headways_sf, IMPT_URL("/mobility_transit/freguesias_headways.gpkg"), delete_dsn = TRUE)
st_write(freguesia_headways, IMPT_URL("/mobility_transit/freguesias_headways.csv"))

municipios_headways = aggregated_transit_times_for_geometry(
  municipios |> st_join(pois_transit_headways_population, join = st_intersects) |> 
    st_drop_geometry() |>
    group_by(municipio)
)
municipios_headways_sf = municipios |> left_join(municipios_headways, by = c("municipio"))
mapview(municipios_headways_sf, zcol="weighted_frequency_peak")
mapview(municipios_headways_sf, zcol="weighted_waiting_time_peak")
mapview(municipios_headways_sf, zcol="weighted_waiting_time_day")

st_write(municipios_headways_sf, IMPT_URL("/mobility_transit/municipios_headways.gpkg"), delete_dsn = TRUE)
st_write(municipios_headways, IMPT_URL("/mobility_transit/municipios_headways.csv"))
