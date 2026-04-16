# ATTENTION! Make sure to run 00_data_load.R and 06_results_load.R before running this script -------------------------------------------------


# SSH authentication ------------------------------------------------------
# When updated, data should be uploaded to the IST serve through SSH, to serve the web dashboard
# install.packages("ssh")
library(ssh)
session <- ssh_connect("ist1108284@sigma.ist.utl.pt")
print(session)

# 2. Associate base layers to nuts  -------------------------------------------------
# grid = impt_read("/geo/grelha_h3_r8.gpkg")
# freguesias = impt_read("/geo/freguesias_2024_unique.gpkg")
# municipios = impt_read("/geo/municipios_2024.gpkg")
#
# freguesias = freguesias |>
#   mutate(
#     nuts2=ifelse(nuts2=="Grande Lisboa", "PT1B", "PT1A"),
#     group_id = substr(dtmnfr, 1, 4)
#   ) |>
#   rename(
#     id = dtmnfr,
#     name = freguesia,
#     region_id = nuts2
#   ) |>
#   select(-area_ha)
# freguesias
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

# mun_nuts = impt_read("mun_nuts.csv", root = "useful_data") |> rename(nuts=nuts_id, id=mun_id,municipio=name)
# mun_nuts
# municipios <- municipios |>
#   left_join(mun_nuts, by = "municipio") |>
#   rename(name=municipio, region_id=nuts)
# municipios
# mapview(municipios, zcol="nuts")
# freguesias = freguesias |> select(-municipio)
#
# grid_centroids = st_centroid(grid)
# grid_nuts = st_join(grid_centroids, freguesias |> select(id, region_id) |> rename(group_id=id), join = st_within) |>
#   st_drop_geometry()
# grid_nuts

# write.csv(
#   grid_nuts |> st_drop_geometry() |> rename(grid_id=id, freg_id=group_id, nuts_id=region_id) |>
#   left_join(freguesias |> st_drop_geometry() |> select(id, group_id) |> rename(freg_id=id, mun_id=group_id), by="freg_id"),
#   "useful_data/grid_nuts.csv",
#   row.names = FALSE
# )
#
# grid = grid |>
#   left_join(grid_nuts, by = "id") |>
#   filter(!is.na(region_id)) |>
#   mutate(name=id)
# grid
# mapview(grid, zcol="nuts")
#
# impt_write(grid, "dashboard_data/grid.gpkg")
# impt_write(freguesias, "dashboard_data/freguesias.gpkg")
# impt_write(municipios, "dashboard_data/municipios.gpkg")

grid <- impt_read("/dashboard_data/grid.gpkg")
freguesias <- impt_read("/dashboard_data/freguesias.gpkg")
municipios <- impt_read("/dashboard_data/municipios.gpkg")
mun_nuts <- impt_read("mun_nuts.csv", root = "useful_data") |> rename(nuts = nuts_id, id = mun_id, municipio = name)
freg_nuts <- impt_read("freguesias_nuts.csv", root = "useful_data") |> rename(id = freg_id, group_id = mun_id, freguesia = name, region_id = nuts_id)

# 3.1 Merge with global results  -------------------------------------------------
# Attention! Run 06_results.R before running this section, to have the global results dataframes available in the environment

modes <- c("", "_bike", "_walk", "_pt", "_car")

municipios_aggregated <- municipios
for (m in modes) {
  message("Mode ", m, "...")

  impt <- impt_read(sprintf("/impt/results/IMPT_PCA_and_Entropy_Scores%s_municipio.csv", m)) |>
    rename(id = mun_id) |>
    mutate(
      # All columns that start with "IMPT_", mutate to 100-.x
      across(starts_with("IMPT_"), ~ 100 - .)
    )
  impt <- impt |>
    rename_with(~ paste0(., m), contains("_Index"))
  municipios_aggregated <- municipios_aggregated |> left_join(impt, by = "id")
}
names(municipios_aggregated)

freguesias_aggregated <- freguesias
for (m in modes) {
  message("Mode ", m, "...")

  impt <- impt_read(sprintf("/impt/results/IMPT_PCA_and_Entropy_Scores%s_freguesia.csv", m)) |>
    rename(id = dtmnfr) |>
    mutate(
      # All columns that start with "IMPT_", mutate to 100-.x
      across(starts_with("IMPT_"), ~ 100 - .)
    )

  impt <- impt |>
    rename_with(~ paste0(., m), contains("_Index"))

  freguesias_aggregated <- freguesias_aggregated |> left_join(impt |> mutate(id = as.character(id)), by = "id")
}
names(freguesias_aggregated)

grid_aggregated <- grid
for (m in modes) {
  message("Mode ", m, "...")

  impt <- impt_read(sprintf("/impt/results/IMPT_PCA_and_Entropy_Scores%s_grid.csv", m)) |>
    rename(id = grid_id) |>
    mutate(
      # All columns that start with "IMPT_", mutate to 100-.x
      across(starts_with("IMPT_"), ~ 100 - .)
    )

  impt <- impt |>
    rename_with(~ paste0(., m), contains("_Index"))

  grid_aggregated <- grid_aggregated |> left_join(impt, by = "id")
}
names(grid_aggregated)

# 3.2 Merge with dimensions indicators  -------------------------------------------------
grid_aggregated <- grid_aggregated |>
  # Accessibility
  left_join(
    grid_accessibility |>
      select(-starts_with("n_")) |> # Remove cols that start with n_ (duplicating pois data)
      rename_with(~ paste0("census_", .), c(-starts_with("pois_"), -id, -starts_with("access_"))),
    by = "id"
  ) |> # Accessibility + census data
  # Mobility
  left_join(
    grid_mobility_costs |>
      select(id, starts_with("mobility_cost")), # Ignore n_ pois repeated from accessibility and census data
    by = "id"
  ) |>
  left_join(
    grid_commuting |>
      rename(id = id_grid_origin) |>
      select(-starts_with("PNA_"), -starts_with("trips_na")) |>
      rename_with(~ paste0("mobility_commuting_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_transit |>
      rename_with(~ paste0("mobility_transit_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_transfers |>
      rename(id = id_grid_origin) |>
      select(id, weighted_mean_transfers) |>
      rename(mobility_transit_commuting_mean_transfers = weighted_mean_transfers),
    by = "id"
  ) |>
  left_join(
    grid_stop_coverage |>
      select(-X) |>
      rename(id = grid_id) |>
      rename_with(~ paste0("mobility_stop_coverage_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_shared_mobility |>
      select(-X) |>
      rename_with(~ paste0("mobility_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_mobility_infrastructure |>
      select(-X) |>
      rename_with(~ paste0("mobility_infrastructure_", .), -id),
    by = "id"
  ) |>
  # Affordability
  left_join(
    grid_affordability_car |>
      rename(id = id_grid_origin) |>
      rename_with(~ paste0("affordability_car_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_affordability_pt_single_fare |>
      rename(id = id_grid_origin) |>
      rename_with(~ paste0("affordability_pt_no_pass_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_affordability_pt_pass |>
      rename(id = id_grid_origin) |>
      rename_with(~ paste0("affordability_pt_pass_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_affordability_composite |>
      rename(id = grid_id) |>
      select(id, transp_inc_comp_nav, transp_inc_comp_sf, h_transp_inc_comp_nav, h_transp_inc_comp_sf) |>
      rename_with(~ paste0("affordability_", .), -id),
    by = "id"
  ) |>
  # Safety
  left_join(
    grid_safety |>
      rename(id = grid_id) |>
      rename_with(~ paste0("safety_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_safety_inner |>
      rename(id = grid_id) |>
      rename(total_veiculos_car = veh_motorizado, total_veiculos_bike = veh_bicicleta, total_veiculos_walk = veh_peoes) |>
      rename_with(~ paste0("safety_inner_", .), -id),
    by = "id"
  ) |>
  # Additional indicators
  left_join(
    grid_pois |>
      rename_with(~ paste0("pois_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_modal_share_census |>
      select(-dicofre, -dtmnfr) |>
      rename_with(~ paste0("modal_census_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_veh_ownership |>
      select(-dicofre) |>
      rename_with(~ paste0("veh_ownership_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_census_income |>
      rename(id = grid_id) |>
      rename_with(~ paste0("census_income_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_census_landuse |>
      rename_with(~ paste0("census_landuse_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_modal_share_imob |>
      rename(id = grid_id) |>
      rename_with(~ paste0("modal_imob_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_access_gap_time |>
      rename(id = grid_id) |>
      rename_with(~ paste0("access_gap_time_", .), -id),
    by = "id"
  ) |>
  left_join(
    grid_access_gap_money |>
      rename(id = grid_id) |>
      rename_with(~ paste0("access_gap_money_", .), -id),
    by = "id"
  )
names(grid_aggregated)

freguesias_aggregated <- freguesias_aggregated |>
  # Accessibility
  left_join(
    freguesia_accessibility |>
      rename(id = dtmnfr) |>
      mutate(id = as.character(id)) |>
      select(-starts_with("n_")) |> # Remove cols that start with n_ (duplicating pois data)
      rename_with(~ paste0("census_", .), c(-starts_with("pois_"), -id, -starts_with("access_"))),
    by = "id"
  ) |> # Accessibility + census data
  # Mobility
  left_join(
    freguesia_mobility_costs |>
      rename(id = dtmnfr) |>
      mutate(id = as.character(id)) |>
      select(id, starts_with("mobility_cost")), # Ignore n_ pois repeated from accessibility and census data
    by = "id"
  ) |>
  left_join(
    freguesia_commuting |>
      rename(id = Origin_dicofre24) |>
      mutate(id = as.character(id)) |>
      select(-starts_with("PNA_"), -starts_with("trips_na")) |>
      rename_with(~ paste0("mobility_commuting_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_transit |>
      rename(id = dtmnfr) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("mobility_transit_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_transfers |>
      rename(id = Origin_dicofre24) |>
      mutate(id = as.character(id)) |>
      select(id, weighted_mean_transfers) |>
      rename(mobility_transit_commuting_mean_transfers = weighted_mean_transfers),
    by = "id"
  ) |>
  left_join(
    freguesia_stop_coverage |>
      select(-X) |>
      left_join(freg_nuts |> select(id, freguesia), by = "freguesia") |>
      select(-freguesia) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("mobility_stop_coverage_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_shared_mobility |>
      select(-X) |>
      rename(id = dtmnfr) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("mobility_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_mobility_infrastructure |>
      select(-X) |>
      rename(id = dtmnfr) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("mobility_infrastructure_", .), -id),
    by = "id"
  ) |>
  # Affordability
  left_join(
    freguesia_affordability_car |>
      rename(id = Origin_dicofre24) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("affordability_car_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_affordability_pt_single_fare |>
      rename(id = Origin_dicofre24) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("affordability_pt_no_pass_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_affordability_pt_pass |>
      rename(id = Origin_dicofre24) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("affordability_pt_pass_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_affordability_composite |>
      rename(id = dtmnfr) |>
      mutate(id = as.character(id)) |>
      select(id, transp_inc_comp_nav, transp_inc_comp_sf, h_transp_inc_comp_nav, h_transp_inc_comp_sf) |>
      rename_with(~ paste0("affordability_", .), -id),
    by = "id"
  ) |>
  # Safety
  left_join(
    freguesia_safety |>
      rename(id = freg_id) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("safety_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_safety_inner |>
      rename(id = freg_id) |>
      mutate(id = as.character(id)) |>
      rename(total_veiculos_car = veh_motorizado, total_veiculos_bike = veh_bicicleta, total_veiculos_walk = veh_peoes) |>
      rename_with(~ paste0("safety_inner_", .), -id),
    by = "id"
  ) |>
  # Additional indicators
  left_join(
    freguesia_pois |>
      rename(id = dtmnfr) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("pois_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_modal_share_census |>
      rename(id = dtmnfr) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("modal_census_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_veh_ownership |>
      rename(id = dicofre) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("veh_ownership_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_census_income |>
      rename(id = freg_id) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("census_income_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_census_landuse |>
      rename(id = freg_id) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("census_landuse_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_modal_share_imob |>
      rename(id = freg_id) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("modal_imob_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_access_gap_time |>
      rename(id = freg_id) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("access_gap_time_", .), -id),
    by = "id"
  ) |>
  left_join(
    freguesia_access_gap_money |>
      rename(id = freg_id) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("access_gap_money_", .), -id),
    by = "id"
  )
names(freguesias_aggregated)


municipios_aggregated <- municipios_aggregated |>
  # Accessibility
  left_join(
    municipio_accessibility |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      select(-starts_with("n_")) |> # Remove cols that start with n_ (duplicating pois data)
      rename_with(~ paste0("census_", .), c(-starts_with("pois_"), -id, -starts_with("access_"))),
    by = "id"
  ) |> # Accessibility + census data
  # Mobility
  left_join(
    municipio_mobility_costs |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      select(id, starts_with("mobility_cost")), # Ignore n_ pois repeated from accessibility and census data
    by = "id"
  ) |>
  left_join(
    municipio_commuting |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      select(-starts_with("PNA_"), -starts_with("trips_na")) |>
      rename_with(~ paste0("mobility_commuting_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_transit |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("mobility_transit_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_transfers |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      select(id, weighted_mean_transfers) |>
      rename(mobility_transit_commuting_mean_transfers = weighted_mean_transfers),
    by = "id"
  ) |>
  left_join(
    municipio_stop_coverage |>
      select(-X) |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("mobility_stop_coverage_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_shared_mobility |>
      select(-X) |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("mobility_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_mobility_infrastructure |>
      select(-X) |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("mobility_infrastructure_", .), -id),
    by = "id"
  ) |>
  # Affordability
  left_join(
    municipio_affordability_car |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("affordability_car_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_affordability_pt_single_fare |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("affordability_pt_no_pass_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_affordability_pt_pass |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("affordability_pt_pass_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_affordability_composite |>
      rename(id = mun_id) |>
      select(id, transp_inc_comp_nav, transp_inc_comp_sf, h_transp_inc_comp_nav, h_transp_inc_comp_sf) |>
      rename_with(~ paste0("affordability_", .), -id),
    by = "id"
  ) |>
  # Safety
  left_join(
    municipio_safety |>
      rename(id = mun_id) |>
      rename_with(~ paste0("safety_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_safety_inner |>
      rename(id = mun_id) |>
      rename(total_veiculos_car = veh_motorizado, total_veiculos_bike = veh_bicicleta, total_veiculos_walk = veh_peoes) |>
      rename_with(~ paste0("safety_inner_", .), -id),
    by = "id"
  ) |>
  # Additional indicators
  left_join(
    municipio_pois |>
      left_join(mun_nuts |> select(id, municipio), by = "municipio") |>
      select(-municipio) |>
      rename_with(~ paste0("pois_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_modal_share_census |>
      rename(id = mun_id) |>
      rename_with(~ paste0("modal_census_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_veh_ownership |>
      rename_with(~ paste0("veh_ownership_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_census_income |>
      rename(id = mun_id) |>
      rename_with(~ paste0("census_income_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_census_landuse |>
      rename(id = mun_id) |>
      rename_with(~ paste0("census_landuse_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_modal_share_imob |>
      rename(id = mun_id) |>
      rename_with(~ paste0("modal_imob_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_access_gap_time |>
      rename(id = mun_id) |>
      rename_with(~ paste0("access_gap_time_", .), -id),
    by = "id"
  ) |>
  left_join(
    municipio_access_gap_money |>
      rename(id = mun_id) |>
      rename_with(~ paste0("access_gap_money_", .), -id),
    by = "id"
  )
names(municipios_aggregated)


# Normalize data ----------------------------------------------------------

# Rename "União das Freguesias de" to "UF" at freguesias_aggregated
freguesias_aggregated <- freguesias_aggregated |>
  mutate(name = gsub("União das freguesias de ", "UF ", name)) |>
  mutate(name = gsub("União das freguesias do ", "UF ", name))

# Round all numeric values to 2 digits
grid_aggregated <- grid_aggregated |>
  mutate(across(where(is.numeric), ~ round(., 2)))
freguesias_aggregated <- freguesias_aggregated |>
  mutate(across(where(is.numeric), ~ round(., 2)))
municipios_aggregated <- municipios_aggregated |>
  mutate(across(where(is.numeric), ~ round(., 2)))

# Replace "_nav" with "_pass" on column names, to be compatible with dashboard filters
# Example: "daily_car_count_nav" becomes "daily_car_count_pass"
replace_anywhere <- list(
  # From, to
  c("navegante", "pass"),
  c("singlefare", "no_pass"),
  c("sf", "no_pass"),
  c("nav", "pass"),
  c("carro", "car"),
  c("private_vehicle", "car"),
  c("peoes", "walk"),
  c("bicicleta", "bike"),
  c("transit", "pt")
)
for (replacement in replace_anywhere) {
  names(grid_aggregated) <- gsub(paste0("^(.*)", replacement[1], "(.*)$"), paste0("\\1", replacement[2], "\\2"), names(grid_aggregated))
  names(freguesias_aggregated) <- gsub(paste0("^(.*)", replacement[1], "(.*)$"), paste0("\\1", replacement[2], "\\2"), names(freguesias_aggregated))
  names(municipios_aggregated) <- gsub(paste0("^(.*)", replacement[1], "(.*)$"), paste0("\\1", replacement[2], "\\2"), names(municipios_aggregated))
}

# Col names with "_mode_" in the middle, should end with "_mode" to be compatible with dashboard filters
# Example: "daily_car_count" becomes "daily_count_car"
# names(freguesias_aggregated) = gsub("^(.*)_car_(.*)$", "\\1_\\2_car", names(freguesias_aggregated))
modes <- list(
  # From, to
  c("car", "car"),
  c("bike", "bike"),
  c("walk", "walk"),
  c("pt", "pt"),
  # pass and no_pass should come after pt, to append it in the end of variable name!
  c("no_pass", "no_pass"), # no_pass before pass, to avoid duplicated change!
  c("pass", "pass")
)

for (mode in modes) {
  names(grid_aggregated) <- gsub(paste0("^(.*)_", mode[1], "_(.*)$"), paste0("\\1_\\2_", mode[2]), names(grid_aggregated))
  names(freguesias_aggregated) <- gsub(paste0("^(.*)_", mode[1], "_(.*)$"), paste0("\\1_\\2_", mode[2]), names(freguesias_aggregated))
  names(municipios_aggregated) <- gsub(paste0("^(.*)_", mode[1], "_(.*)$"), paste0("\\1_\\2_", mode[2]), names(municipios_aggregated))
}


# Extra: Champions --------------------------------------------------------

dimensions <- c("Accessibility", "Mobility", "Safety")
champions <- data.frame()
for (d in dimensions) {
  content <- impt_read(paste("/impt/pca_scores/Champions_", d, ".csv", sep = "")) |>
    mutate(position = row_number()) |>
    select(-Contribution) |>
    rename(metric = Indicator)
  content$dimension <- paste(d, "Index", sep = "_")

  champions <- rbind(champions, content)
}
champions_list <- list()
for (d in dimensions) {
  d_name <- paste(d, "Index", sep = "_")
  champions_list[[d_name]] <- list()
  for (i in 1:nrow(champions |> filter(dimension == paste(d, "Index", sep = "_")))) {
    champions_list[[d_name]][[i]] <- champions |>
      filter(dimension == paste(d, "Index", sep = "_")) |>
      select(metric) |>
      slice(i) |>
      pull()
  }
}

# 4. Export to geojson -------------------------------------------------
output_dir <- "dashboard_data"

output_location <- impt_write(grid_aggregated, paste(output_dir, "grid_aggregated.geojson", sep = "/"))
impt_write(freguesias_aggregated, paste(output_dir, "freguesias_aggregated.geojson", sep = "/"))
impt_write(municipios_aggregated, paste(output_dir, "municipios_aggregated.geojson", sep = "/"))

impt_write(grid_aggregated |> st_drop_geometry(), paste(output_dir, "grid_aggregated.csv", sep = "/"))
impt_write(freguesias_aggregated |> st_drop_geometry(), paste(output_dir, "freguesias_aggregated.csv", sep = "/"))
impt_write(municipios_aggregated |> st_drop_geometry(), paste(output_dir, "municipios_aggregated.csv", sep = "/"))

json_str <- jsonlite::toJSON(champions_list, auto_unbox = TRUE, pretty = TRUE)
impt_write(json_str, paste(output_dir, "champions.json", sep = "/"))

length(names(grid_aggregated)) # 831
length(names(freguesias_aggregated)) # 1057
length(names(municipios_aggregated)) # 1048

freguesias_aggregated <- impt_read(paste(output_dir, "freguesias_aggregated.csv", sep = "/"))
grid_aggregated <- impt_read(paste(output_dir, "grid_aggregated.csv", sep = "/"))
munipios_aggregated <- impt_read(paste(output_dir, "municipios_aggregated.csv", sep = "/"))

municipio_names <- names(municipios_aggregated) |> data.frame()
freguesia_names <- names(freguesias_aggregated) |> data.frame()
grid_names <- names(grid_aggregated) |> data.frame()

# names(freguesias_aggregated)
# freguesias_aggregated |>
#   # select(starts_with("IMPT_score_pca_geom")) |> # Filter those that start with "IMPT_score_pca_geom"
#   # Filter those that contain "affordability"
#   names()

# Upload to IST server for dashboard  ----------------------------------------------------
output_location <- dirname(output_location)
files <- c(
  "grid_aggregated.geojson", "freguesias_aggregated.geojson", "municipios_aggregated.geojson",
  "grid_aggregated.csv", "freguesias_aggregated.csv", "municipios_aggregated.csv"
)
for (f in files) {
  scp_upload(session, paste(output_location, f, sep = "/"), to = paste("/afs/.ist.utl.pt/groups/ushift/web/content/impt", f, sep = "/"))
}
scp_upload(session, paste(output_location, "champions.json", sep = "/"), to = paste("/afs/.ist.utl.pt/groups/ushift/web/content/impt", "champions.json", sep = "/"))
# scp_upload(session, "/data/geo/cos_builtarea.geojson", to = paste("/afs/.ist.utl.pt/groups/ushift/web/content/impt", "cos_builtarea.geojson", sep = "/"))
