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
cos_residential_aml = st_read("/data/IMPT/geo/cos_residential_aml.gpkg")


# with grid ---------------------------------------------------------------

grid = st_read("/data/IMPT/geo/grelha_h3_r8.gpkg")
census_bgri = st_read("/data/IMPT/original/BGRI21_170.gpkg")

mapview(cos_residential_aml, zcol = "COS23_n4_L") + 
  mapview(grid, alpha = 0.3)
mapview(cos_residential_aml, zcol = "COS23_n4_L") + 
  mapview(census_bgri, alpha = 0.2)


#### test with densymetric grid
# --- STEP 1: CRS & CLEANING ---
# Use EPSG:3763 for Portugal (Meters)
target_crs <- 3763 

census_bgri <- st_transform(census_bgri, target_crs) %>% st_make_valid()
grid_meters        <- st_transform(grid, target_crs)
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
  group_by(BGRI2021) %>% # Use your actual ID column name here
  mutate(total_bgri_weight_score = sum(frag_weight_score)) %>%
  ungroup() %>%
  filter(total_bgri_weight_score > 0) %>% # ignore empty
  # 3. Distribute population based on fragment's share of the BGRI score
  mutate(
    pop_fraction = frag_weight_score / total_bgri_weight_score,
    fragment_pop = N_INDIVIDUOS * pop_fraction
  ) %>%
  # 4. Aggregate to H3 Grid
  group_by(index) %>% # Use your actual H3 index column name
  summarise(
    total_pop = sum(fragment_pop, na.rm = TRUE),
    weighted_urban_index = mean(weight, na.rm = TRUE),
    .groups = "drop"
  )

# Join back to original grid geometry to visualize
grid_final_map <- grid_meters %>%
  left_join(st_drop_geometry(grid_populated), by = "index")