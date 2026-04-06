# Travel time to nearest opportunities
# Purpose     Compute travel time to first (1,2,3) opportunities of each category, by mode 
# Scale       hex, parish, municipality
# Issue       -

library(mapview)

output_dir = "/mobility_costs/r8"
if(!dir.exists(IMPT_URL(output_dir))) {
  dir.create(IMPT_URL(output_dir), recursive = TRUE)
}

# Travel time matrix ---------------------------------------------------------------
# Defined in 04_accessibility.R


# POIs ---------------------------------------------------------------
# Defined in 04_accessibility.R


# Grid population ---------------------------------------------------------------
# Defined in 04_accessibility.R

# Number of opportunities -------------------------------------------------

# 1. Count opportunities accessible
for(i in 1:length(pois_list)) {
  pois_name = pois_list[[i]][[1]]
  pois_data = pois_list[[i]][[2]]
  if ("id" %in% colnames(pois_data)) {
    pois_data = pois_data |> select(-id)
  }
  message(paste("Processing POI category:", pois_name))

  grid_points <- st_join(grid, pois_data, join = st_intersects) |>
    group_by(id) |>
    # Sum the dummy column. 'na.rm = TRUE' treats the NAs (non-matches) as 0.
    summarise(!!pois_name := sum(n, na.rm = TRUE)) |> # mapview(grid_points, zcol=pois_name)
    st_drop_geometry()
  
  
  for(z in 1:length(ttm_list)) {
    ttm_name = ttm_list[[z]][[1]]
    ttm_data = readRDS_remote(IMPT_URL(sprintf("%s/%s", ttm_root, ttm_list[[z]][[2]]))) |>
      mutate(
        from_id = as.integer(from_id),
        to_id = as.integer(to_id)
      )
    message(paste("> Processing travel time matrix:", ttm_name))
    
    grid_points_mode = grid_points
    colname = paste0("mobility_cost_", pois_name, "_", ttm_name)
    for(n in c(1,2,3)) {
      colname_n = paste(colname, "_n", n, sep = "")
      mobility_cost <- accessibility::cost_to_closest(
        travel_matrix = ttm_data, 
        land_use_data = grid_points,
        opportunity = pois_name,
        travel_cost = 'travel_time_p50',
        active=TRUE,
        n=n
      ) |> rename(
        !!colname_n := travel_time_p50
      )
      
      grid_points_mode = grid_points_mode |> 
        left_join(mobility_cost, by = "id")
    }
    ncolname = paste("n_", pois_name, sep = "")
    grid_points_mode = grid_points_mode |> rename(!!ncolname := sym(pois_name))
    # mapview(grid |> left_join(grid_points_mode), zcol = ncolname)
    write.csv(grid_points_mode, IMPT_URL(sprintf("%s/%s.csv", output_dir, colname)), row.names = FALSE)
    message(paste(">>> Saved result for", colname, ", procedding with next..."))
  }
}

# 2. Aggregate by parish, population weighted
# accessibility_modes and accessibility_masures Defined in 04_accessibility.R

grid_mobility = grid_population 
freguesia_mobility = freguesias |> 
  st_transform(st_crs(grid_mobility)) |> 
  st_join(grid_mobility, join = st_intersects) |>
  st_drop_geometry() |>
  group_by(dtmnfr) |>
  summarise(
    # Demographics
    buildings = sum(buildings),
    families = sum(families),
    kids = sum(kids),
    elder = sum(elder),
    residents = sum(residents)
  ) |>
  # Get geometry back
  left_join(freguesias |> select(dtmnfr, geom), by = "dtmnfr") |>
  st_as_sf()

municipio_mobility = municipios |> 
  st_transform(st_crs(grid_mobility)) |> 
  st_join(grid_mobility, join = st_intersects) |>
  st_drop_geometry() |>
  group_by(municipio) |>
  summarise(
    # Demographics
    buildings = sum(buildings),
    families = sum(families),
    kids = sum(kids),
    elder = sum(elder),
    residents = sum(residents)
  ) |>
  # Get geometry back
  left_join(municipios |> select(municipio, geom), by = "municipio") |>
  st_as_sf()

for (i in 1:length(accessibility_measures)) {
  poi_name = accessibility_measures[[i]][[1]]
  census_col = accessibility_measures[[i]][[2]]
  modes_times = accessibility_measures[[i]][[3]]
  message(paste("Processing accessibility for POI category", poi_name, "census", census_col))
  
  for (m in 1:length(modes_times)) {
    mode_name = modes_times[[m]][[1]]
    message(paste("> Processing mode", mode_name))
  
    colname = paste0("mobility_cost_", poi_name, "_", mode_name)
    ncolname = paste("n_", poi_name, sep = "")
    mobility = read.csv(IMPT_URL(sprintf("%s/%s.csv", output_dir, colname)))
    if (!(ncolname %in% names(grid_mobility))) { # to avoid error when same pois for different populations
      grid_mobility = grid_mobility |>
          left_join(mobility |> select(id, sym(ncolname)), by = "id") |> 
          mutate(!!ncolname := round(get(ncolname), digits=2))
    }
    for (n in c(1,2,3)) {
      colname_n = paste(colname, "_n", n, sep = "")
      
      if (!(colname_n %in% names(grid_mobility))) {
        grid_mobility = grid_mobility |>
          left_join(mobility |> select(id, sym(colname_n)), by = "id") |> 
          # Replace Inf by NA
          mutate(!!colname_n := ifelse(is.infinite(get(colname_n)), NA, get(colname_n))) |>
          mutate(!!colname_n := round(get(colname_n), digits=2))
      }

      freguesia_mob = freguesia_mobility |> 
        select(dtmnfr, geom) |>
        st_join(grid_mobility, join = st_intersects) |>
        st_drop_geometry() |>
        group_by(dtmnfr)
      
      municipio_mob = municipio_mobility |>
        select(municipio, geom) |>
        st_join(grid_mobility, join = st_intersects) |>
        st_drop_geometry() |>
        group_by(municipio)
      
      colname_population = paste(colname_n, "_", census_col, sep="")
      
      freguesia_mob = freguesia_mob |>
        summarise(
          # Weighted means by population
          !!colname_population := weighted.mean(get(colname_n), get(census_col), na.rm=TRUE),
          # N sum
          !!ncolname := sum(get(ncolname), na.rm=TRUE)
        ) 
      municipio_mob = municipio_mob |> summarise(
        # Weighted means by population
        !!colname_population := weighted.mean(get(colname_n), get(census_col), na.rm=TRUE),
        # N sum
        !!ncolname := sum(get(ncolname), na.rm=TRUE)
      ) 
      
      freguesia_mob = freguesia_mob |> mutate(!!colname_population := round(get(colname_population), digits=2))
      municipio_mob = municipio_mob |> mutate(!!colname_population := round(get(colname_population), digits=2))
      freguesia_mob = freguesia_mob |> mutate(!!ncolname := round(get(ncolname), digits=2))  
      municipio_mob = municipio_mob |> mutate(!!ncolname := round(get(ncolname), digits=2))
      
      if (ncolname %in% names(freguesia_mobility)) {
        freguesia_mob = freguesia_mob |> select(-sym(ncolname))
      }
      if (ncolname %in% names(municipio_mobility)) {
        municipio_mob = municipio_mob |> select(-sym(ncolname))
      }
      
      freguesia_mobility = freguesia_mobility |> 
        left_join(freguesia_mob, by = "dtmnfr")
      
      municipio_mobility = municipio_mobility |>
        left_join(municipio_mob, by= "municipio")
      # View(freguesia_mobility|>st_drop_geometry())
    }
  }
}

st_write(freguesia_mobility, IMPT_URL(sprintf("%s/mobility_freguesia.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(freguesia_mobility |> st_drop_geometry(), IMPT_URL(sprintf("%s/mobility_freguesia.csv", output_dir)), row.names = FALSE) 
st_write(municipio_mobility, IMPT_URL(sprintf("%s/mobility_municipio.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(municipio_mobility |> st_drop_geometry(), IMPT_URL(sprintf("%s/mobility_municipio.csv", output_dir)), row.names = FALSE) 
st_write(grid_mobility, IMPT_URL(sprintf("%s/mobility_grid.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(grid_mobility |> st_drop_geometry(), IMPT_URL(sprintf("%s/mobility_grid.csv", output_dir)), row.names = FALSE)
# 
# mapview(freguesia_mobility, zcol = "mobility_cost_health_walk_n1_residents")
# mapview(freguesia_mobility, zcol = "mobility_cost_health_walk_n1_elder")
# mapview(freguesia_mobility, zcol = "mobility_cost_groceries_walk_n1_residents")
# mapview(freguesia_mobility, zcol = "mobility_cost_groceries_car_n1_residents")
# mapview(freguesia_mobility, zcol = "mobility_cost_recreation_walk_n1_residents")
# mapview(freguesia_mobility, zcol = "mobility_cost_recreation_transit_2t_n1_residents")
# mapview(grid_mobility, zcol = "mobility_cost_recreation_transit_2t_n1")
# mapview(grid_mobility, zcol = "mobility_cost_recreation_walk_n1")
