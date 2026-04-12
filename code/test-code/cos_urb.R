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


# explore cos
cos_raw = st_read("/data/IMPT/original/COS2023v1-S2.gpkg")
unique(cos_raw$COS23_n4_L)
unique(cos_raw$COS23_n4_C)

cos_residential = cos_raw |> filter(COS23_n4_L %in% c(
  "Áreas edificadas residenciais contínuas predominantemente verticais",
  "Áreas edificadas residenciais contínuas predominantemente horizontais",
  "Áreas edificadas residenciais descontínuas",
  "Áreas edificadas residenciais descontínuas esparsas"
))

mapview(cos_residential)


# filter aml
clip_bbox = st_read("/data/IMPT/geo/clipping_boundary.geojson")
cos_residential_aml = cos_residential |> st_transform(crs = st_crs(clip_bbox))
cos_residential_aml = cos_residential_aml[clip_bbox,]
cos_residential_aml = cos_residential_aml |> select(-ID)
mapview(cos_residential_aml, zcol = "COS23_n4_L")

# export
st_write(cos_residential_aml, "/data/IMPT/geo/cos_residential_aml.gpkg", delete_dsn = TRUE)



# with grid ---------------------------------------------------------------

grid = st_read("/data/IMPT/geo/grelha_h3_r8.gpkg")

mapview(cos_residential_aml, zcol = "COS23_n4_L") + 
  mapview(grid, alpha = 0.3)

census_bgri = st_read("/data/IMPT/original/BGRI21_170.gpkg")

mapview(cos_residential_aml, zcol = "COS23_n4_L") + 
  mapview(census_bgri, alpha = 0.2)


#### test with densymetric grid

library(sf)
library(dplyr)

# 1. Ensure all datasets are in the same Projected CRS (e.g., UTM)
# This is CRITICAL for accurate area calculations.
census_bgri  <- st_transform(census_bgri, 4326) |> st_make_valid()

# 2. Pre-process COS: Assign weights to density classes
# Example: Higher density built volume gets a higher weight if desired,
# or simply treat all urbanized area as 1 and non-urban as 0.
cos_habitable <- cos_residential_aml %>%
  mutate(weight = case_when(
    COS23_n4_L == "Áreas edificadas residenciais contínuas predominantemente verticais" ~ 3.0,
    COS23_n4_L == "Áreas edificadas residenciais contínuas predominantemente horizontais"  ~ 1.0,
    COS23_n4_L == "Áreas edificadas residenciais descontínuas"  ~ 0.6,
    TRUE ~ 0.3
  ))

# 3. Intersect Census with COS to find 'Urbanized Census' zones
# This creates fragments that know both their BGRI ID and their land-use weight
bgri_urban <- st_intersection(census_bgri, cos_habitable) %>%
  mutate(fragment_area = st_area(.)) ## THIS TAKES SOME TIME

# 4. Intersect the weighted fragments with the H3 Grid
# This breaks the urbanized census data into the H3 hex shapes
final_fragments <- st_intersection(bgri_urban, grid) %>%
  mutate(intersect_area = st_area(.))

# 5. Calculate the Population Distribution
# We calculate how much of the BGRI's "weighted area" falls into this specific Hex
grid_populated <- final_fragments %>%
  group_by(bgri_id) %>% 
  mutate(total_weighted_area = sum(intersect_area * weight)) %>%
  ungroup() %>%
  mutate(
    # The proportion of the BGRI population assigned to this hex fragment
    pop_fraction = (intersect_area * weight) / total_weighted_area,
    fragment_pop = population * pop_fraction
  ) %>%
  # 6. Aggregate back to the H3 Grid level
  group_by(h3_index) %>%
  summarise(
    total_pop = sum(fragment_pop, na.rm = TRUE),
    avg_density = mean(weight, na.rm = TRUE), # Optional: carry over COS info
    .groups = "drop"
  )
