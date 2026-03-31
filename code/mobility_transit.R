# PT Waiting Times + night/weekend service availability
# Purpose     Compute PT frequencies and headways
# Scale       hex, parish, municipality
# Issue       - 


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
  filter(!is.na(population_served) & !is.na(frequency_peak)) |> 
  mutate(
    across(
      starts_with("frequency_"),
      ~ ifelse(is.na(.x), 0, .x)
    )
  )
summary(pois_transit_headways_population)
# mapview(pois_transit_headways_population, zcol="population_served")
# mapview(pois_transit_headways_population, zcol="frequency_night")

# Aggregate headway (average) by grid, parish and county, weighted by population served
aggregated_transit_times_for_geometry = function(grid) {
  return (
    grid |> 
      summarise(
        # Frequencies and headways, weighted
        across(
          starts_with("headway_"),
          ~ round(weighted.mean(.x, population_served, na.rm=TRUE), digits=2),
          .names = "weighted_{.col}"
        ),
        across(
          starts_with("frequency_"),
          ~ round(weighted.mean(.x, population_served, na.rm=TRUE), digits=2),
          .names = "weighted_{.col}"
        ),
        # Total values
        across(
          starts_with("frequency_"),
          ~ sum(.x, na.rm=TRUE),
          .names = "total_{.col}"
        ),
        total_population_served = sum(population_served, na.rm=TRUE),
        n_stops = n()
      ) |> 
      mutate(
        # Convert headway, in seconds, in waiting time
        across(
          starts_with("weighted_headway_"),
          ~ round(.x / 2 / 60, digits=2), # in minutes
          .names = "weighted_waiting_time_{sub('weighted_headway_', '', .col)}"
        ),
        # Relate night and weekend service availability
        weighted_frequency_reduction_night = round((weighted_frequency_peak - weighted_frequency_night) / weighted_frequency_peak * 100, digits=2),
        weighted_frequency_reduction_weekend = round((weighted_frequency_peak - weighted_frequency_weekend) / weighted_frequency_peak * 100, digits=2),
        frequency_reduction_night = round((total_frequency_peak - total_frequency_night) / total_frequency_peak * 100, digits=2),
        frequency_reduction_weekend = round((total_frequency_peak - total_frequency_weekend) / total_frequency_peak * 100, digits=2)
      )
  )
}
grid_headways = aggregated_transit_times_for_geometry(
  grid |> st_join(pois_transit_headways_population, join = st_intersects) |> 
  st_drop_geometry() |>
  group_by(id)
)
summary(grid_headways)
  
grid_headways_sf = grid |> left_join(grid_headways, by = "id")
mapview(grid_headways_sf, zcol="weighted_frequency_peak")
mapview(grid_headways_sf, zcol="weighted_waiting_time_peak")
mapview(grid_headways_sf, zcol="weighted_waiting_time_peak") + mapview(pois_transit_headways_population)
mapview(grid_headways_sf, zcol="n_stops")
mapview(grid_headways_sf, zcol="frequency_reduction_night")
mapview(grid_headways_sf, zcol="frequency_reduction_weekend")
mapview(grid_headways_sf |> mutate(total_population_served_str = ifelse(total_population_served>0, "ANY", "NONE")), zcol="total_population_served_str")

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
mapview(freguesia_headways_sf, zcol="frequency_reduction_night")
mapview(freguesia_headways_sf, zcol="frequency_reduction_weekend")


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
mapview(municipios_headways_sf, zcol="frequency_reduction_night") 
mapview(municipios_headways_sf, zcol="weighted_frequency_reduction_night") 
mapview(municipios_headways_sf, zcol="frequency_reduction_weekend")
mapview(municipios_headways_sf, zcol="weighted_frequency_reduction_weekend")

st_write(municipios_headways_sf, IMPT_URL("/mobility_transit/municipios_headways.gpkg"), delete_dsn = TRUE)
st_write(municipios_headways, IMPT_URL("/mobility_transit/municipios_headways.csv"))
