library(sf)
library(tidyverse)
library(mapview)

# download data
# link_cos = "geo2.dgterritorio.gov.pt/cos/S2/COS2023/COS2023v1-S2-gpkg.zip" #856 MB
# temp <- "/data/IMPT/original/cos2023.zip"
# #Increase timeout to 600 seconds (10 minutes)
# options(timeout = 600)
# download.file(link_cos, temp, mode = "wb", )
# unzip(temp, exdir = "/data/IMPT/original/")


# # explore cos
# cos_raw = st_read("/data/IMPT/original/COS2023v1-S2.gpkg")
# unique(cos_raw$COS23_n4_L)
# unique(cos_raw$COS23_n4_C)
#
# cos_residential = cos_raw |> filter(COS23_n4_L %in% c(
#   "Áreas edificadas residenciais contínuas predominantemente verticais",
#   "Áreas edificadas residenciais contínuas predominantemente horizontais",
#   "Áreas edificadas residenciais descontínuas",
#   "Áreas edificadas residenciais descontínuas esparsas"
# ))
#
# # mapview(cos_residential)
#
#
# # filter aml
# clip_bbox = st_read("/data/IMPT/geo/clipping_boundary.geojson")
# cos_residential_aml = cos_residential |> st_transform(crs = st_crs(clip_bbox))
# cos_residential_aml = cos_residential_aml[clip_bbox,]
# cos_residential_aml = cos_residential_aml |> select(-ID)
# mapview(cos_residential_aml, zcol = "COS23_n4_L")

# export
# st_write(cos_residential_aml, "/data/IMPT/geo/cos_residential_aml.gpkg", delete_dsn = TRUE)

# with grid ---------------------------------------------------------------

cos_residential_aml <- st_read(IMPT_URL("/geo/cos_residential_aml.gpkg"))
grid <- st_read(IMPT_URL("/geo/grelha_h3_r8.gpkg"))
census_bgri <- st_read(IMPT_URL("/original/BGRI21_170.gpkg"))

mapview(cos_residential_aml, zcol = "COS23_n4_L") +
  mapview(grid, alpha = 0.3)
mapview(cos_residential_aml, zcol = "COS23_n4_L") +
  mapview(census_bgri, alpha = 0.2)


#### test with densymetric grid
# --- STEP 1: CRS & CLEANING ---
# Use EPSG:3763 for Portugal (Meters)
target_crs <- 3763

census_bgri <- st_transform(census_bgri, target_crs) %>% st_make_valid()
grid_meters <- st_transform(grid, target_crs)
cos_habitable <- cos_residential_aml %>%
  rename(classification = COS23_n4_L) %>%
  mutate(weight = case_when(
    classification == "Áreas edificadas residenciais contínuas predominantemente verticais" ~ 3.0,
    classification == "Áreas edificadas residenciais contínuas predominantemente horizontais" ~ 1.0,
    classification == "Áreas edificadas residenciais descontínuas" ~ 0.6,
    TRUE ~ 0.3
  )) %>%
  st_transform(target_crs)

# --- STEP 2: DISSOLVE COS ---
cos_clean <- cos_habitable %>%
  group_by(classification) %>%
  summarise(weight = first(weight)) %>%
  st_simplify(dTolerance = 1) %>%
  st_make_valid()

# --- STEP 3: SPATIAL JOIN LOGIC ---
# Filter grid to active area
grid_active <- grid_meters[st_intersects(grid_meters, census_bgri, sparse = FALSE) %>% apply(1, any), ]

# Intersect Grid with COS (Weighted Grid)
grid_weighted <- st_intersection(grid_active, cos_clean)

# Simplify Census to Polygons for speed
census_simple <- census_bgri %>%
  st_cast("POLYGON") %>%
  st_make_valid()

# Final Intersection (The "Chopping")
final_intersection <- st_intersection(grid_weighted, census_simple) %>%
  mutate(frag_area = as.numeric(st_area(.))) # Convert to numeric to avoid units issues

# --- STEP 4: POPULATION REDISTRIBUTION ---
grid_populated <- final_intersection %>%
  # 1. Calculate the 'weighted score' for every fragment
  mutate(frag_weight_score = frag_area * weight) %>%
  # 2. Sum weights per original Census block (BGRI)
  group_by(BGRI2021) %>% # grid_id
  mutate(total_bgri_weight_score = sum(frag_weight_score)) %>%
  ungroup() %>%
  filter(total_bgri_weight_score > 0) %>% # ignore empty
  # 3. Distribute population based on fragment's share of the BGRI score
  mutate(
    pop_fraction = frag_weight_score / total_bgri_weight_score,
    fragment_pop = N_INDIVIDUOS * pop_fraction
  ) %>%
  # 4. Aggregate to H3 Grid
  group_by(id) %>% # Use your actual H3 index column name
  summarise(
    total_pop = sum(fragment_pop, na.rm = TRUE),
    weighted_urban_index = mean(weight, na.rm = TRUE),
    .groups = "drop"
  )
# the results are still fragments, not hex cells


# Check
sum(census_bgri$N_INDIVIDUOS, na.rm = TRUE) # 2870208
sum(grid_populated$total_pop, na.rm = TRUE) # 2853546
# difference = 16662 # better than the census centroids!! (was 19.562)
# 0.58 %

# 1. Summarize the population data (removing geometry for speed)
h3_pop_table <- final_intersection %>%
  st_drop_geometry() %>%
  mutate(frag_weight_score = frag_area * weight) %>%
  group_by(BGRI2021) %>%
  mutate(total_bgri_weight_score = sum(frag_weight_score)) %>%
  ungroup() %>%
  filter(total_bgri_weight_score > 0) %>%
  mutate(
    pop_fraction = frag_weight_score / total_bgri_weight_score,
    fragment_buildings = N_EDIFICIOS_CLASSICOS * pop_fraction,
    fragment_buildings_pre1945 = N_EDIFICIOS_CONSTR_ANTES_1945 * pop_fraction,
    fragment_population = N_INDIVIDUOS * pop_fraction,
    fragment_households = N_NUCLEOS_FAMILIARES * pop_fraction, # households
    fragment_youth = N_INDIVIDUOS_0_14 * pop_fraction,
    fragment_elderly = N_INDIVIDUOS_65_OU_MAIS * pop_fraction,
    fragment_adults = (N_INDIVIDUOS_15_24 + N_INDIVIDUOS_25_64) * pop_fraction,
    fragment_women = N_INDIVIDUOS_M * pop_fraction
  ) %>%
  # NOW group by the H3 index to combine all fragments in that cell
  group_by(id) %>%
  summarise(
    buildings = sum(fragment_buildings, na.rm = TRUE),
    buildings_pre1945 = sum(fragment_buildings_pre1945, na.rm = TRUE),
    population = sum(fragment_population, na.rm = TRUE),
    households = sum(fragment_households, na.rm = TRUE),
    youth = sum(fragment_youth, na.rm = TRUE),
    elderly = sum(fragment_elderly, na.rm = TRUE),
    adults = sum(fragment_adults, na.rm = TRUE),
    women = sum(fragment_women, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Join back to the original Grid to get full Hexagon geometries
grid_final <- grid %>%
  left_join(h3_pop_table, by = "id") %>%
  mutate(households = if_else(households > 0 & households < 1, 1, households)) %>% # Prevent 0.5 households
  mutate(
    # Fill NAs with 0 first so the math doesn't break
    across(c(population, youth, elderly, women, buildings, buildings_pre1945), ~ replace_na(.x, 0)),
    area_km2 = as.numeric(st_area(.)) / 1000000,
    population_density = population / area_km2,
    youth_ratio = if_else(population > 0, round(youth / population * 100, 2), 0),
    elderly_ratio = if_else(population > 0, round(elderly / population * 100, 2), 0),
    youth_dependency = if_else(population > 0, round(youth / adults * 100, 2), 0),
    elderly_dependency = if_else(population > 0, round(elderly / adults * 100, 2), 0),
    women_percentage = if_else(population > 0, round(women / population * 100, 2), 0),
    pp_hh_avg = ifelse(population > 0, ifelse(households > 0, round(population / households, 2), population), 0),
    buildings_pre1945_percentage = if_else(buildings > 0, round(buildings_pre1945 / buildings * 100, 2), 0)
  ) %>%
  mutate( # solve to avoud inf and nan
    youth_dependency = if_else(is.nan(youth_dependency), 0, youth_dependency),
    elderly_dependency = if_else(is.infinite(elderly_dependency), 100, elderly_dependency)
  ) |>
  mutate( # Solve pp_hh_avg too high because low number of households
    pp_hh_avg = if_else(households < 3, NA_real_, pp_hh_avg),
    pp_hh_avg = if_else(pp_hh_avg > 50, NA_real_, pp_hh_avg)
  ) |>
  mutate(across(c(buildings, buildings_pre1945, population, youth, elderly, adults, women), ceiling)) %>%
  st_transform(4326)

sum(grid_final$population) # 2854660
# difference = 15548 # even better
mapview(grid_final |> filter(population > 0), zcol = "population")
mapview(grid_final |> filter(population > 0), zcol = "elderly_dependency")

summary(grid_final$population_density) # sounds good

# export
st_write(grid_final, IMPT_URL("/landuse/grid_with_cos.gpkg"), delete_dsn = TRUE)
write.csv(grid_final |> st_drop_geometry(), IMPT_URL("/landuse/grid_with_cos.csv"), row.names = FALSE)
