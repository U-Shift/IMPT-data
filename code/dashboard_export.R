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
    region_id = nuts2
  ) |>
  select(-area_ha)
freguesias
# mapview(freguesias, zcol="group_id")
# write.csv(
#   freguesias |> 
#     select(id,name,region_id,group_id) |> 
#     st_drop_geometry() |>
#     rename(freg_id=id, mun_id=group_id, nuts_id=region_id), 
#   "useful_data/freguesias_nuts.csv", 
#   row.names = FALSE
# )

# mun_nuts <- freguesias |>
#   st_drop_geometry() |>
#   group_by(group_id) |>
#   summarise(
#     nuts = first(region_id),
#     municipio = first(municipio),
#     .groups = "drop"
#   ) |>
#   rename(id=group_id)
# write.csv(mun_nuts |> rename(mun_id=id, nuts_id=nuts, name=municipio), "useful_data/mun_nuts.csv", row.names = FALSE)

mun_nuts = read.csv("useful_data/mun_nuts.csv") |> rename(nuts=nuts_id, id=mun_id,municipio=name)
mun_nuts
municipios <- municipios |>
  left_join(mun_nuts, by = "municipio") |>
  rename(name=municipio, region_id=nuts)
municipios
# mapview(municipios, zcol="nuts")
freguesias = freguesias |> select(-municipio)

grid_centroids = st_centroid(grid)
grid_nuts = st_join(grid_centroids, freguesias |> select(id, region_id) |> rename(group_id=id), join = st_within) |>
  st_drop_geometry()
grid_nuts

# write.csv(
#   grid_nuts |> st_drop_geometry() |> rename(grid_id=id, freg_id=group_id, nuts_id=region_id) |>
#   left_join(freguesias |> st_drop_geometry() |> select(id, group_id) |> rename(freg_id=id, mun_id=group_id), by="freg_id"),
#   "useful_data/grid_nuts.csv", 
#   row.names = FALSE
# )

grid = grid |>
  left_join(grid_nuts, by = "id") |>
  filter(!is.na(region_id)) |>
  mutate(name=id)
grid
# mapview(grid, zcol="nuts")

# 3.1 Merge with global results  -------------------------------------------------
# Attention! Run results_visualizer.R before running this section, to have the global results dataframes available in the environment

impt_pca = read.csv(IMPT_URL("results_aggregated/20260330/IMPT_PCA_and_Entropy_Scores.csv")) |>
  rename(Affordability_Index_no_nav = MS_AFF_PRE_NAV, Affordability_Index_nav = MS_AFF_POST_NAV) |>
  
impt_pca_bike = read.csv(IMPT_URL("results_aggregated/20260330/IMPT_PCA_and_Entropy_Scores_bike.csv")) |>
  rename_with(~ paste0(., "_bike"), ends_with("_Index"))
impt_pca_walk = read.csv(IMPT_URL("results_aggregated/20260330/IMPT_PCA_and_Entropy_Scores_walk.csv")) |> 
  rename_with(~ paste0(., "_walk"), ends_with("_Index"))
impt_pca_pt = read.csv(IMPT_URL("results_aggregated/20260330/IMPT_PCA_and_Entropy_Scores_pt.csv")) |> 
  rename_with(~ paste0(., "_pt"), ends_with("_Index")) |>
  rename(Affordability_Index_pt_no_nav = h_transp_inc_pt.x, Affordability_Index_pt_nav = h_transp_inc_pt.y)
impt_pca_car = read.csv(IMPT_URL("results_aggregated/20260330/IMPT_PCA_and_Entropy_Scores_car.csv")) |> 
  rename_with(~ paste0(., "_car"), ends_with("_Index")) |>
  rename(Affordability_Index_car=h_transp_inc_car)

freguesias_aggregated = freguesias |> 
  left_join(
    impt_pca |> 
      mutate(dtmnfr = as.character(dtmnfr)) |>
      st_drop_geometry()
    ,by=c("id"="dtmnfr")
  ) |> 
  left_join(
    impt_pca_bike |> 
      mutate(dtmnfr = as.character(dtmnfr)) |>
      st_drop_geometry()
    ,by=c("id"="dtmnfr")
  ) |> 
  left_join(
    impt_pca_walk |> 
      mutate(dtmnfr = as.character(dtmnfr)) |>
      st_drop_geometry()
    ,by=c("id"="dtmnfr")
  ) |> 
  left_join(
    impt_pca_pt |> 
      mutate(dtmnfr = as.character(dtmnfr)) |>
      st_drop_geometry()
    ,by=c("id"="dtmnfr")
  ) |> 
  left_join(
    impt_pca_car |> 
      mutate(dtmnfr = as.character(dtmnfr)) |>
      st_drop_geometry()
    ,by=c("id"="dtmnfr")
  ) |>
  mutate(
    # All columns that start with "IMPT_", mutate to 100-.x
    across(starts_with("IMPT_"), ~ 100 - .)
  )
names(freguesias_aggregated)
summary(freguesias_aggregated)

# 3.2 Merge with dimensions indicators  -------------------------------------------------
# Attention! Run results.R before running this section, to have the indicators dataframes available in the environment
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
  ) |> 
  left_join(
    grid_affordability_car |> 
      st_drop_geometry() |>
      rename_with(~ paste0("affordability_car_", .), -id),
    by="id"
  ) |>
  left_join(
    grid_affordability_pt_single_fare |> 
      st_drop_geometry() |>
      rename_with(~ paste0("affordability_pt_single_fare_", .), -id),
    by="id"
  ) |>
  left_join(
    grid_affordability_pt_pass |> 
      st_drop_geometry() |>
      rename_with(~ paste0("affordability_pt_pass_", .), -id),
    by="id"
  )
names(grid_aggregated)

freguesias_aggregated = freguesias_aggregated |> 
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
  ) |>
  left_join(
    freguesia_affordability_car |> 
      st_drop_geometry() |>
      rename(id=dtmnfr) |>
      mutate(id=as.character(id)) |>
      rename_with(~ paste0("affordability_car_", .), -id),
    by="id"
  ) |>
  left_join(
    freguesia_affordability_pt_single_fare |> 
      st_drop_geometry() |>
      rename(id=dtmnfr) |>
      mutate(id=as.character(id)) |>
      rename_with(~ paste0("affordability_pt_single_fare_", .), -id),
    by="id"
  ) |>
  left_join(
    freguesia_affordability_pt_pass |> 
      st_drop_geometry() |>
      rename(id=dtmnfr) |>
      mutate(id=as.character(id)) |>
      rename_with(~ paste0("affordability_pt_pass_", .), -id),
    by="id"
  )
names(freguesias_aggregated)


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
  ) |>
  left_join(
    municipio_affordability_car |> 
      st_drop_geometry() |> 
      left_join(mun_nuts |> select(id, municipio), by="municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("affordability_car_", .), -id),
    by="id"
  ) |>
  left_join(
    municipio_affordability_pt_single_fare |> 
      st_drop_geometry() |> 
      left_join(mun_nuts |> select(id, municipio), by="municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("affordability_pt_single_fare_", .), -id),
    by="id"
  ) |>
  left_join(
    municipio_affordability_pt_pass |> 
      st_drop_geometry() |> 
      left_join(mun_nuts |> select(id, municipio), by="municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("affordability_pt_pass_", .), -id),
    by="id"
  )
names(municipios_aggregated)

# Normalize data (round all numeric values to 2 digits)
grid_aggregated = grid_aggregated |> 
  mutate(across(where(is.numeric), ~ round(., 2)))
freguesias_aggregated = freguesias_aggregated |> 
  mutate(across(where(is.numeric), ~ round(., 2)))
municipios_aggregated = municipios_aggregated |> 
  mutate(across(where(is.numeric), ~ round(., 2)))

# 4. Export to geojson -------------------------------------------------
output_dir = "dashboard_data"

st_write(grid_aggregated, IMPT_URL(paste(output_dir, "grid_aggregated.geojson", sep="/")), delete_dsn = TRUE)
st_write(freguesias_aggregated, IMPT_URL(paste(output_dir, "freguesias_aggregated.geojson", sep="/")), delete_dsn = TRUE)
st_write(municipios_aggregated, IMPT_URL(paste(output_dir, "municipios_aggregated.geojson", sep="/")), delete_dsn = TRUE)

write.csv(grid_aggregated |> st_drop_geometry(), IMPT_URL(paste(output_dir, "grid_aggregated.csv", sep="/")), row.names = FALSE)
write.csv(freguesias_aggregated |> st_drop_geometry(), IMPT_URL(paste(output_dir, "freguesias_aggregated.csv", sep="/")), row.names = FALSE)
write.csv(municipios_aggregated |> st_drop_geometry(), IMPT_URL(paste(output_dir, "municipios_aggregated.csv", sep="/")), row.names = FALSE)

names(freguesias_aggregated)
freguesias_aggregated |> 
  # select(starts_with("IMPT_score_pca_geom")) |> # Filter those that start with "IMPT_score_pca_geom"
  # Filter those that contain "affordability"
  names()
