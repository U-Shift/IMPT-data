# Assuming pois_list from 04_access_opportunities.R is defined

freguesias_pois = freguesias |> select(dtmnfr) |> st_drop_geometry()
municipios_pois = municipios |> select(municipio) |> st_drop_geometry()
grid_pois = grid |> select(id) |> st_drop_geometry()

for(i in 1:length(pois_list)) {
  pois_name = pois_list[[i]][[1]]
  pois_data = pois_list[[i]][[2]]
  message(sprintf("Processing %s...", pois_name))
  
  col_name = paste("n_", pois_name, sep = "")
  
  if ("id" %in% colnames(pois_data)) {
    pois_data = pois_data |> select(-id)
  } 
  
  # Count occurrences of each POI in scale
  pois_freguesia = st_join(freguesias, pois_data |> st_transform(st_crs(freguesias)), left = FALSE) |>
    st_drop_geometry() |>
    group_by(dtmnfr) |>
    summarise(!!col_name := n())
  
  pois_municipio = st_join(municipios, pois_data |> st_transform(st_crs(municipios)), left = FALSE) |>
    st_drop_geometry() |>
    group_by(municipio) |>
    summarise(!!col_name := n())
  
  pois_grid = st_join(grid, pois_data |> st_transform(st_crs(grid)), left = FALSE) |>
    st_drop_geometry() |>
    group_by(id) |>
    summarise(!!col_name := n())
  
  freguesias_pois = freguesias_pois |> left_join(pois_freguesia, by="dtmnfr")
  municipios_pois = municipios_pois |> left_join(pois_municipio, by="municipio")
  grid_pois = grid_pois |> left_join(pois_grid, by="id")
}

# mapview::mapview(freguesias |> left_join(freguesias_pois, by="dtmnfr"), zcol = "n_health", legend = TRUE)
# mapview::mapview(municipios |> left_join(municipios_pois, by="municipio"), zcol = "n_health", legend = TRUE)
# mapview::mapview(grid |> left_join(grid_pois, by="id"), zcol = "n_health", legend = TRUE) 
# mapview::mapview(grid |> left_join(grid_pois, by="id"), zcol = "n_jobs", legend = TRUE) 

write.csv(freguesias_pois, IMPT_URL("pois/freguesias_pois.csv"), row.names = FALSE)
write.csv(municipios_pois, IMPT_URL("pois/municipios_pois.csv"), row.names = FALSE)
write.csv(grid_pois, IMPT_URL("pois/grid_pois.csv"), row.names = FALSE)
