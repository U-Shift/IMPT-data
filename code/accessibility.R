library(mapview)

output_dir = "/accessibility/r8"

# Travel time matrix ---------------------------------------------------------------

# ttm_bike_60 = readRDS_remote(IMPT_URL("/ttm/ttm_h3_res9/ttm_bicycle_60min_202602040800.rds"))
# nrow(ttm_bike_60 |> filter(travel_time_p50<=55))
# ttm_bike_120 = readRDS_remote(IMPT_URL("/ttm/ttm_h3_res9/ttm_bicycle_120min_202602040800.rds"))
# nrow(ttm_bike_120)
# nrow(ttm_bike_120 |> filter(travel_time_p50<=55))
# View(ttm_bike_120)
ttm_root = "/ttm/ttm_h3_res8"
ttm_list = list(
  # Mode, Travel time matrix file, Time cutoffs to consider
  list("walk", "ttm_walk_60min_202602040800.rds", list(15,30,45)),
  list("bike", "ttm_bicycle_60min_202602040800.rds", list(15,30,45)),
  list("car", "ttm_car_60min_202602040800.rds", list(5,10,15,30,45)),
  list("transit_1t", "ttm_transit_60min_202602040800_1transfers.rds", list(5,10,15,30,45)),
  list("transit_2t", "ttm_transit_60min_202602040800_2transfers.rds", list(5,10,15,30,45))
)

ttm_list = list(
  # Mode, Travel time matrix file, Time cutoffs to consider
  list("walk", "ttm_walk_60min_202602040800.rds", list(45)),
  list("bike", "ttm_bicycle_60min_202602040800.rds", list(45)),
  list("car", "ttm_car_60min_202602040800.rds", list(45)),
  list("transit_1t", "ttm_transit_60min_202602040800_1transfers.rds", list(45)),
  list("transit_2t", "ttm_transit_60min_202602040800_2transfers.rds", list(45))
)

# POIs ---------------------------------------------------------------
pois_health = st_read(IMPT_URL("/pois/healthcare.gpkg")) |> mutate(n=1)
pois_supermarket = st_read(IMPT_URL("/pois/supermarket.gpkg")) |> mutate(n=1)
pois_green= st_read(IMPT_URL("/pois/green.gpkg")) |> mutate(n=1)
pois_recreation = st_read(IMPT_URL("/pois/recreation.gpkg")) |> mutate(n=1)
pois_schools = st_read(IMPT_URL("/pois/schools.gpkg")) |> mutate(n=1)
pois_jobs = st_read(IMPT_URL("/pois/pois_jobs_imob_jt50.gpkg")) |> rename(n=trips)

pois_list = list(
  # POI type, sf data.frame with points 
  # list("health", pois_health),
  # list("health_primary", pois_health |> filter(type=="Centro de Saúde")),
  # list("health_hospital", pois_health |> filter(type=="Hospital")),
  # list("groceries", pois_supermarket),
  # list("greenspaces", pois_green),
  # list("recreation", pois_recreation),
  # list("schools", pois_schools),
  # list("schools_primary", pois_schools |> filter(grepl("Cycle", type))),
  list("jobs", pois_jobs)
)


# Grid population ---------------------------------------------------------------
grid_census = census |> st_join(grid, join = st_within)
  
grid_population = grid_census |>
  st_drop_geometry() |>   # drop geometry for counting
  group_by(id.y) |>
  summarise(buildings = sum(N_EDIFICIOS_CLASSICOS),
            families = sum(N_NUCLEOS_FAMILIARES),
            residents = sum(N_INDIVIDUOS),
            kids = sum(N_INDIVIDUOS_0_14), # 0-14
            elder = sum(N_INDIVIDUOS_65_OU_MAIS), # 65+
            male = sum(N_INDIVIDUOS_H),
            female = sum(N_INDIVIDUOS_M)
            ) |> 
  rename(id = id.y) |> 
  filter(!is.na(id)) |>
  ungroup()

grid_population = grid |>
  left_join(grid_population, by = "id") |>
  mutate(across(where(is.numeric), ~tidyr::replace_na(.x, 0)))

# mapview(grid_population, zcol = "residents")
# mapview(grid_population, zcol = "elder")
# mapview(grid_population, zcol = "buildings")

grid_points_population = st_centroid(grid_population)

# Calculations: Number of opportunities -------------------------------------------------

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
    ttm_cutoffs = ttm_list[[z]][[3]]
    message(paste("> Processing travel time matrix:", ttm_name))
    
    for(cutoff in ttm_cutoffs) {
      message(paste(">> Processing cutoff:", cutoff, "minutes"))
      colname = paste0("access_", pois_name, "_", ttm_name, "_", cutoff, "min")
      access_result <- accessibility::cumulative_cutoff(
        travel_matrix = ttm_data, 
        land_use_data = grid_points,
        opportunity = pois_name,
        travel_cost = 'travel_time_p50',
        cutoff = cutoff
      ) |> rename(
        !!colname := sym(pois_name)
      )
      
      ncolname = paste("n_", pois_name, sep = "")
      grid_points_access = grid_points |> 
        left_join(access_result, by = "id") |>
        rename(!!ncolname := sym(pois_name))
        
      # mapview(grid |> left_join(grid_points_access), zcol = colname)
      write.csv(grid_points_access, IMPT_URL(sprintf("%s/%s.csv", output_dir, colname)), row.names = FALSE)
      message(paste(">>> Saved result for", colname, ", procedding with next..."))
    }
  }
}

# 2. Aggregate by parish, population weighted

accessibility_measures = list(
  # POI type, Population relates to, [ Mode, Time ]
  list("health", "residents", list(
    list("walk", 15),
    list("bike", 15),
    list("car", 30),
    list("transit_2t", 30)
  )),
  list("schools_primary", "kids", list(
    list("walk", 15),
    list("bike", 15),
    list("car", 30),
    list("transit_2t", 30)
  )),
  list("greenspaces", "residents", list(
    list("walk", 15),
    list("bike", 15),
    list("car", 10),
    list("transit_2t", 10)
  )),
  list("recreation", "residents", list(
    list("walk", 15),
    list("bike", 15),
    list("car", 10),
    list("transit_2t", 10)
  )),
  list("groceries", "residents", list(
    list("walk", 15),
    list("bike", 15),
    list("car", 15),
    list("transit_2t", 15)
  )),
  list("jobs", "residents", list(
    list("walk", 45),
    list("bike", 45),
    list("car", 45),
    list("transit_2t", 45)
  ))
)


grid_access = grid_population 
freguesia_accessibility = freguesias |> 
  st_transform(st_crs(grid_access)) |> 
  st_join(grid_access, join = st_intersects) |>
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
# mapview(freguesia_accessibility, zcol = "residents")

for (i in 1:length(accessibility_measures)) {
  poi_name = accessibility_measures[[i]][[1]]
  census_col = accessibility_measures[[i]][[2]]
  modes_time = accessibility_measures[[i]][[3]]
  message(paste("Processing accessibility for POI category:", poi_name))
  
  for (m in 1:length(modes_time)) {
    message(paste("> Processing mode and time:", modes_time[[m]][[1]], modes_time[[m]][[2]], "minutes"))
    mode_name = modes_time[[m]][[1]]
    mode_time = modes_time[[m]][[2]]
    
    colname = paste0("access_", poi_name, "_", mode_name, "_", mode_time, "min")
    ncolname = paste("n_", poi_name, sep = "")
    access = read.csv(IMPT_URL(sprintf("%s/%s.csv", output_dir, colname)))
    if (m>1) {
      access = access |> select(-sym(ncolname))
    }
    
    grid_access = grid_access |>
      left_join(access, by = "id")
    
    freguesia_access = freguesia_accessibility |> 
      select(dtmnfr, geom) |>
      st_join(grid_access, join = st_intersects) |>
      st_drop_geometry() |>
      group_by(dtmnfr)
    
    if (m==1) {
      freguesia_access = freguesia_access |>
        summarise(
          # Weighted means by population
          !!colname := weighted.mean(get(colname), get(census_col), na.rm=TRUE),
          # N sum
          !!ncolname := sum(get(ncolname), na.rm=TRUE)
        ) 
    } else {
      freguesia_access = freguesia_access |>
        summarise(
          # Weighted means by population
          !!colname := weighted.mean(get(colname), get(census_col), na.rm=TRUE)
        ) 
    }
    
    freguesia_accessibility = freguesia_accessibility |> 
      left_join(freguesia_access, by = "dtmnfr")
    # View(freguesia_accessibility|>st_drop_geometry())
  }
}

st_write(freguesia_accessibility, IMPT_URL(sprintf("%s/accessibility_freguesia.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(freguesia_accessibility |> st_drop_geometry(), IMPT_URL(sprintf("%s/accessibility_freguesia.csv", output_dir)), row.names = FALSE) 
st_write(grid_access, IMPT_URL(sprintf("%s/accessibility_grid.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(grid_access, IMPT_URL(sprintf("%s/accessibility_grid.csv", output_dir)), row.names = FALSE)

# mapview(freguesia_accessibility, zcol = "access_jobs_walk_45min") +
#   mapview(freguesia_accessibility, zcol = "access_jobs_bike_45min") +
#   mapview(freguesia_accessibility, zcol = "access_jobs_car_45min") +
#   mapview(freguesia_accessibility, zcol = "access_jobs_transit_2t_45min") 
# # 
# mapview(grid_access, zcol = "access_jobs_walk_45min") +
#   mapview(grid_access, zcol = "access_jobs_bike_45min") +
#   mapview(grid_access, zcol = "access_jobs_car_45min") +
#   mapview(grid_access, zcol = "access_jobs_transit_2t_45min")
# #   mapview(pois_health)

# Nearest opportunity -----------------------------------------------------
# 
# cost_to_closest = accessibility::cost_to_closest(
#   travel_matrix = ttm,
#   land_use_data = grid_points,
#   opportunity = 'healthcare',
#   travel_cost = 'travel_time_p50'
# )
