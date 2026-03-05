# 1. Make sure to run data_load.R and results.R before running this script -------------------------------------------------

# 2. Associate base layers to nuts  -------------------------------------------------
grid = st_read(IMPT_URL("/geo/grelha_h3_r8.gpkg"))
freguesias = st_read(IMPT_URL("/geo/freguesias_2024_unique.gpkg"))
municipios = st_read(IMPT_URL("/geo/municipios_2024.gpkg"))

freguesias = freguesias |> 
  mutate(
    nuts2=ifelse(nuts2=="Grande Lisboa", "PT1B", "PT1A"),
    group_id = substr(dtmnfr, 1, 4)
  ) |>
  rename(
    id = dtmnfr,
    name = freguesia,
    nuts = nuts2
  ) |>
  select(-area_ha)
freguesias
# mapview(freguesias, zcol="nuts")

mun_nuts <- freguesias |>
  st_drop_geometry() |>
  group_by(group_id) |>
  summarise(
    nuts = first(nuts),
    municipio = first(municipio),
    .groups = "drop"
  ) |>
  rename(id=group_id)
# write.csv(mun_nuts, "useful_data/mun_nuts.csv", row.names = FALSE)
mun_nuts
municipios <- municipios |>
  left_join(mun_nuts, by = "municipio") |>
  rename(name=municipio)
municipios
# mapview(municipios, zcol="nuts")
freguesias = freguesias |> select(-municipio)

grid_centroids = st_centroid(grid)
grid_nuts = st_join(grid_centroids, freguesias |> select(id, nuts) |> rename(group_id=id), join = st_within) |>
  st_drop_geometry()
grid_nuts
grid = grid |>
  left_join(grid_nuts, by = "id") |>
  filter(!is.na(nuts)) |>
  mutate(name=id)
grid
# mapview(grid, zcol="nuts")


# 3. Merge with several layers  -------------------------------------------------
grid_original = grid
grid_original
# grid = grid_original

grid_aggregated = grid |> 
  left_join(
    grid_accessibility |> 
      st_drop_geometry() |> 
      rename_with(~ paste0("pois_", .), starts_with("n_")) |>
      rename_with(~ paste0("census_", .), c(-starts_with("pois_"), -id, -starts_with("access_")))
    , by="id") |> # Accessibility + census data
  left_join(
    grid_mobility_costs |>
      st_drop_geometry() |>
      select(id, starts_with("mobility_cost")),  # Ignore n_ pois repeated from accessibility and census data
    by="id"
  ) |> 
  left_join(
    grid_commuting |> 
      st_drop_geometry() |> 
      select(-starts_with("PNA_"), -starts_with("trips_na")) |>
      rename_with(~ paste0("mobility_commuting_", .), -id),
    by="id"
  ) |>
  left_join(
    grid_transit |> 
      st_drop_geometry() |>
      rename_with(~ paste0("mobility_transit_", .), -id),
    by="id"
  ) |>
  left_join(
    grid_transfers |>
      st_drop_geometry() |>
      select(id, weighted_mean_transfers) |>
      rename(mobility_transit_commuting_mean_transfers=weighted_mean_transfers),
    by = "id"
  )
names(grid_aggregated)

freguesias_aggregated = freguesias |> 
  left_join(
    freguesia_accessibility |> 
      st_drop_geometry() |> 
      rename(id=dtmnfr) |>
      rename_with(~ paste0("pois_", .), starts_with("n_")) |>
      rename_with(~ paste0("census_", .), c(-starts_with("pois_"), -id, -starts_with("access_")))
    , by="id") |> # Accessibility + census data
  left_join(
    freguesia_mobility_costs |>
      st_drop_geometry() |>
      rename(id=dtmnfr) |>
      select(id, starts_with("mobility_cost")), # Ignore n_ pois repeated from accessibility and census data
    by="id"
  ) |> 
  left_join(
    freguesia_commuting |> 
      st_drop_geometry() |> 
      rename(id=dtmnfr) |>
      select(-starts_with("PNA_"), -starts_with("trips_na")) |>
      rename_with(~ paste0("mobility_commuting_", .), -id),
    by="id"
  ) |>
  left_join(
    freguesia_transit |> 
      st_drop_geometry() |>
      rename(id=dtmnfr) |>
      rename_with(~ paste0("mobility_transit_", .), -id),
    by="id"
  ) |>
  left_join(
    freguesia_transfers |>
      st_drop_geometry() |>
      rename(id=dtmnfr) |>
      select(id, weighted_mean_transfers) |>
      rename(mobility_transit_commuting_mean_transfers=weighted_mean_transfers),
    by = "id"
  )


municipios_aggregated = municipios |> 
  left_join(
    municipio_accessibility |> 
      st_drop_geometry() |> 
      left_join(mun_nuts |> select(id, municipio), by="municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("pois_", .), starts_with("n_")) |>
      rename_with(~ paste0("census_", .), c(-starts_with("pois_"), -id, -starts_with("access_")))
    , by="id") |> # Accessibility + census data
  left_join(
    municipio_mobility_costs |>
      st_drop_geometry() |>
      left_join(mun_nuts |> select(id, municipio), by="municipio") |>
      select(-municipio) |>
      select(id, starts_with("mobility_cost")), # Ignore n_ pois repeated from accessibility and census data
    by="id"
  ) |> 
  left_join(
    municipio_commuting |> 
      st_drop_geometry() |> 
      left_join(mun_nuts |> select(id, municipio), by="municipio") |>
      select(-municipio) |>
      select(-starts_with("PNA_"), -starts_with("trips_na")) |>
      rename_with(~ paste0("mobility_commuting_", .), -id),
    by="id"
  ) |>
  left_join(
    municipio_transit |> 
      st_drop_geometry() |>
      left_join(mun_nuts |> select(id, municipio), by="municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("mobility_transit_", .), -id),
    by="id"
  ) |>
  left_join(
    municipio_transfers |>
      st_drop_geometry() |>
      left_join(mun_nuts |> select(id, municipio), by="municipio") |>
      select(-municipio) |>
      select(id, weighted_mean_transfers) |>
      rename(mobility_transit_commuting_mean_transfers=weighted_mean_transfers),
    by = "id"
  )
names(municipios_aggregated)


# 4. Export to geojson -------------------------------------------------
output_dir = "dashboard_data"

st_write(grid_aggregated, IMPT_URL(paste(output_dir, "grid_aggregated.geojson", sep="/")), delete_dsn = TRUE)
st_write(freguesias_aggregated, IMPT_URL(paste(output_dir, "freguesias_aggregated.geojson", sep="/")), delete_dsn = TRUE)
st_write(municipios_aggregated, IMPT_URL(paste(output_dir, "municipios_aggregated.geojson", sep="/")), delete_dsn = TRUE)

write.csv(grid_aggregated |> st_drop_geometry(), IMPT_URL(paste(output_dir, "grid_aggregated.csv", sep="/")), row.names = FALSE)
write.csv(freguesias_aggregated |> st_drop_geometry(), IMPT_URL(paste(output_dir, "freguesias_aggregated.csv", sep="/")), row.names = FALSE)
write.csv(municipios_aggregated |> st_drop_geometry(), IMPT_URL(paste(output_dir, "municipios_aggregated.csv", sep="/")), row.names = FALSE)
