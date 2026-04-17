# Population served by Public Transit isochrones
# Purpose     Estimate population within walking catchment areas of PT stops,
#             using COS residential building polygons as the spatial population proxy.
#             This is more accurate than the H3 grid because the isochrone may
#             clip through a hex where buildings are concentrated on only one side.
# Method      Dasymetric intersection:
#               COS residential polygons (weighted by building density class)
#               × BGRI census blocks (population source)
#               × Isochrone polygon (catchment area)
#             Population served = Σ N_INDIVIDUOS × (weighted building area inside iso
#                                                   / total weighted building area in BGRI)
# Scale       Total (AML), hex (H3 r8), freguesia, município
# Mode groups Bus (5 min), Subway/LightRail (10 min), Train/Ferry (15 min), Any PT (union)
# Depends     00a_impt_data_handle.R, 00b_data_load.R, and 04_isochrones_PTstops.R must be run first

library(dplyr)
library(sf)
library(mapview)


# 1. Load data ---------------------------------------------------------------

# COS 2023 residential land-use polygons (clipped to AML)
cos_residential_aml <- impt_read("/geo/cos_residential_aml.gpkg")

# BGRI 2021 census blocks with population counts
census_bgri <- impt_read("/original/BGRI21_170.gpkg")

# Isochrone polygons (dissolved per mode, from 04_isochrones_PTstops.R)
isochrones_bus <- impt_read("/mobility/isochrones_buffers_bus_5min.gpkg")
isochrones_metrolr <- impt_read("/mobility/isochrones_subwaylightrail_10min.gpkg")
isochrones_trainferry <- impt_read("/mobility/isochrones_trainferry_15min.gpkg")

# Administrative boundaries and H3 grid (from 00b_data_load.R)
# grid, freguesias, municipios are expected to already be in environment

target_crs <- 3763 # EPSG:3763 PT-TM06, metric, for accurate area calculations


# 2. Prepare COS building polygons (same logic as 02_census_grid_with_cos.R) --

cos_habitable <- cos_residential_aml |>
  rename(classification = COS23_n4_L) |>
  mutate(weight = case_when(
    classification == "Áreas edificadas residenciais contínuas predominantemente verticais" ~ 3.0,
    classification == "Áreas edificadas residenciais contínuas predominantemente horizontais" ~ 1.0,
    classification == "Áreas edificadas residenciais descontínuas" ~ 0.6,
    TRUE ~ 0.3 # descontínuas esparsas
  )) |>
  st_transform(target_crs)

# Dissolve by class so each class is a single (multi)polygon with its weight.
# This speeds up the intersection while preserving weight differentiation.
cos_clean <- cos_habitable |>
  group_by(classification) |>
  summarise(weight = first(weight)) |>
  st_simplify(dTolerance = 1) |>
  st_make_valid()

# Prepare BGRI: keep only the population fields we need
census_simple <- census_bgri |>
  st_transform(target_crs) |>
  st_make_valid() |>
  st_cast("POLYGON") |> # dissolve any multipolygons for clean intersection
  st_make_valid() |>
  select(BGRI2021, N_INDIVIDUOS)


# 3. Pre-compute COS × BGRI fragments (done once, reused for all modes) ------
#
# Each fragment = a piece of a COS residential polygon clipped to a BGRI block.
# It carries:
#   - the building density weight from COS
#   - the BGRI's total population (N_INDIVIDUOS)
#   - its own area and weight score
#   - the total weight score of its parent BGRI (denominator for pop apportioning)
#
# This is the same logic as 02_census_grid_with_cos.R but without the H3 layer,
# so that the geometry reflects building polygons directly.

message("Computing COS × BGRI fragments (this may take a few minutes)...")

bgri_cos_fragments <- st_intersection(cos_clean, census_simple) %>%
  mutate(frag_area = as.numeric(st_area(.))) %>% # aparently the (.) only works with the old pipe %>%
  filter(frag_area > 0) %>% # drop degenerate slivers
  mutate(frag_weight_score = frag_area * weight) |>
  group_by(BGRI2021) |>
  mutate(total_bgri_weight_score = sum(frag_weight_score)) |>
  ungroup() |>
  filter(total_bgri_weight_score > 0) # ignore BGRIs with no habitable area

# Sense check: total reconstructed population should be close to census total
bgri_cos_fragments <- bgri_cos_fragments |>
  mutate(fragment_population = N_INDIVIDUOS * (frag_weight_score / total_bgri_weight_score))

  sum(census_bgri$N_INDIVIDUOS, na.rm = TRUE) # 2870208
  round(sum(bgri_cos_fragments$fragment_population, na.rm = TRUE)) #2862157
  round(sum(census_bgri$N_INDIVIDUOS, na.rm = TRUE) -
    sum(bgri_cos_fragments$fragment_population, na.rm = TRUE)) #8051
  abs(sum(census_bgri$N_INDIVIDUOS, na.rm = TRUE) -
    sum(bgri_cos_fragments$fragment_population, na.rm = TRUE)) /
    sum(census_bgri$N_INDIVIDUOS, na.rm = TRUE) * 100 #0.28%



# 4. Helper: population inside an isochrone ----------------------------------
#
# Clips the pre-computed COS × BGRI building fragments to the isochrone boundary.
# For fragments that are partially inside, the overlapping area is computed and
# used to proportionally reduce the population share.
#
# Returns a sf with one row per clipped fragment, carrying:
#   - BGRI2021, N_INDIVIDUOS, weight, total_bgri_weight_score
#   - frag_area_in_iso, frag_weight_score_in_iso
#   - pop_in_iso  (the population apportioned to this building piece inside the iso)

compute_pop_in_isochrone <- function(building_frags, isochrone) {
  iso_m <- st_transform(isochrone, target_crs) |> st_make_valid()

  # Fast pre-filter: only fragments that touch the isochrone
  frags_hit <- building_frags[lengths(st_intersects(building_frags, iso_m)) > 0, ]

  if (nrow(frags_hit) == 0) {
    message("  -> No building fragments intersect this isochrone.")
    return(NULL)
  }

  # Clip fragments to isochrone geometry (handles partial overlaps at boundary)
  frags_in_iso <- st_intersection(frags_hit, iso_m) %>%
    mutate(
      frag_area_in_iso = as.numeric(st_area(.)),
      frag_weight_score_in_iso = frag_area_in_iso * weight,
      # Population apportioned: re-weight by the inside-iso area
      pop_in_iso = N_INDIVIDUOS * (frag_weight_score_in_iso / total_bgri_weight_score)
    ) |>
    filter(frag_area_in_iso > 0)

  frags_in_iso
}


# 5. Compute for each mode + union -------------------------------------------

# Union isochrone: any PT mode (used to avoid double-counting)
message("Building union isochrone (any PT)...")
isochrones_all <- bind_rows(
  isochrones_bus |> st_transform(target_crs) |> st_make_valid(),
  isochrones_metrolr |> st_transform(target_crs) |> st_make_valid(),
  isochrones_trainferry |> st_transform(target_crs) |> st_make_valid()
) |>
  summarise(geometry = st_union(geom)) |>
  st_make_valid()

message("Clipping building fragments to Train/Ferry isochrone (15 min)...")
frags_trainferry <- compute_pop_in_isochrone(bgri_cos_fragments, isochrones_trainferry)
message("Clipping building fragments to Subway/LightRail isochrone (10 min)...")
frags_metrolr <- compute_pop_in_isochrone(bgri_cos_fragments, isochrones_metrolr)
message("Clipping building fragments to Bus isochrone (5 min)...")
frags_bus <- compute_pop_in_isochrone(bgri_cos_fragments, isochrones_bus) # takes some time
message("Clipping building fragments to all-PT isochrone (union)...")
frags_all <- compute_pop_in_isochrone(bgri_cos_fragments, isochrones_all) # takes some time

# Total population served (AML level) — quick summary
for (nm in c("bus", "metrolr", "trainferry", "all")) {
  frags <- get(paste0("frags_", nm))
  message(sprintf(
    "  [%s] Population served: %s",
    nm, format(round(sum(frags$pop_in_iso, na.rm = TRUE)), big.mark = ",")
  ))
}
# [bus] Population served: 2,233,115
# [metrolr] Population served: 252,554
# [trainferry] Population served: 460,860
# [all] Population served: 2,319,048
# [all_perc] Population served: 80.79 %


# 6. Helper: aggregate clipped fragments to a spatial unit -------------------
#
# Given the building fragments inside an isochrone, spatially joins them to
# an administrative/grid layer and sums pop_in_iso per unit.

aggregate_to_layer <- function(frags_in_iso, layer_sf, group_col) {
  if (is.null(frags_in_iso) || nrow(frags_in_iso) == 0) {
    return(tibble(!!group_col := character(0), pop_served = numeric(0)))
  }

  layer_m <- st_transform(layer_sf, target_crs) |> st_make_valid()
  
  # To avoid double-counting population when building fragments cross unit boundaries
  # (especially hex boundaries), we represent each fragment by its centroid.
  # This ensures each fragment's population is assigned to exactly one spatial unit.
  frags_pts <- frags_in_iso |>
    st_centroid() |>
    st_make_valid()

  layer_m |>
    st_join(frags_pts |> select(pop_in_iso), join = st_intersects) |>
    st_drop_geometry() |>
    group_by(across(all_of(group_col))) |>
    summarise(pop_served = sum(pop_in_iso, na.rm = TRUE), .groups = "drop")
}


# 7. Grid-level (H3 r8) output -----------------------------------------------

message("Aggregating to H3 grid...")

grid_bus <- aggregate_to_layer(frags_bus, grid, "id")
grid_metrolr <- aggregate_to_layer(frags_metrolr, grid, "id")
grid_trainferry <- aggregate_to_layer(frags_trainferry, grid, "id")
grid_all <- aggregate_to_layer(frags_all, grid, "id")

# Also get total population per hex from grid_with_cos (for % calculation)
grid_with_cos <- impt_read("/landuse/grid_with_cos.gpkg")

grid_pt_pop <- grid_with_cos |>
  select(id, population) |>
  left_join(grid_bus |> rename(pop_pt_bus = pop_served), by = "id") |>
  left_join(grid_metrolr |> rename(pop_pt_metrolr = pop_served), by = "id") |>
  left_join(grid_trainferry |> rename(pop_pt_trainferry = pop_served), by = "id") |>
  left_join(grid_all |> rename(pop_pt_all = pop_served), by = "id") |>
  mutate(
    across(starts_with("pop_pt_"), ~ replace_na(.x, 0)),
    across(starts_with("pop_pt_"), round),
    pt_served_bus        = pop_pt_bus > 0,
    pt_served_metrolr    = pop_pt_metrolr > 0,
    pt_served_trainferry = pop_pt_trainferry > 0,
    pt_served_all        = pop_pt_all > 0,
    pct_pt_bus           = ifelse(population > 0, round(pop_pt_bus / population, 4), 0),
    pct_pt_metrolr       = ifelse(population > 0, round(pop_pt_metrolr / population, 4), 0),
    pct_pt_trainferry    = ifelse(population > 0, round(pop_pt_trainferry / population, 4), 0),
    pct_pt_all           = ifelse(population > 0, round(pop_pt_all / population, 4), 0)
  )

# mapview(grid_pt_pop |> filter(pt_served_all), zcol = "pop_pt_all")
# mapview(grid_pt_pop |> filter(pt_served_bus), zcol = "pop_pt_bus")
# mapview(grid_pt_pop |> filter(pt_served_metrolr), zcol = "pop_pt_metrolr")


# 8. Freguesia-level output --------------------------------------------------

message("Aggregating to Freguesia...")

freg_bus <- aggregate_to_layer(frags_bus, freguesias, "dtmnfr")
freg_metrolr <- aggregate_to_layer(frags_metrolr, freguesias, "dtmnfr")
freg_trainferry <- aggregate_to_layer(frags_trainferry, freguesias, "dtmnfr")
freg_all <- aggregate_to_layer(frags_all, freguesias, "dtmnfr")

# Total population per freguesia from the same building-based method
freg_total <- aggregate_to_layer(
  bgri_cos_fragments |> mutate(pop_in_iso = fragment_population),
  freguesias, "dtmnfr"
) |>
  rename(pop_total = pop_served)

freguesias_pt_pop <- freg_total |>
  left_join(freg_bus |> rename(pop_pt_bus = pop_served), by = "dtmnfr") |>
  left_join(freg_metrolr |> rename(pop_pt_metrolr = pop_served), by = "dtmnfr") |>
  left_join(freg_trainferry |> rename(pop_pt_trainferry = pop_served), by = "dtmnfr") |>
  left_join(freg_all |> rename(pop_pt_all = pop_served), by = "dtmnfr") |>
  mutate(
    across(starts_with("pop_pt_"), ~ replace_na(.x, 0)),
    across(starts_with("pop_pt_"), round),
    pct_pt_bus        = round(pop_pt_bus / pop_total, 4),
    pct_pt_metrolr    = round(pop_pt_metrolr / pop_total, 4),
    pct_pt_trainferry = round(pop_pt_trainferry / pop_total, 4),
    pct_pt_all        = round(pop_pt_all / pop_total, 4)
  )

# mapview(freguesias |> left_join(freguesias_pt_pop, by = "dtmnfr"), zcol = "pct_pt_all")


# 9. Município-level output --------------------------------------------------

message("Aggregating to Município...")

mun_bus <- aggregate_to_layer(frags_bus, municipios, "municipio")
mun_metrolr <- aggregate_to_layer(frags_metrolr, municipios, "municipio")
mun_trainferry <- aggregate_to_layer(frags_trainferry, municipios, "municipio")
mun_all <- aggregate_to_layer(frags_all, municipios, "municipio")

mun_total <- aggregate_to_layer(
  bgri_cos_fragments |> mutate(pop_in_iso = fragment_population),
  municipios, "municipio"
) |>
  rename(pop_total = pop_served)

municipios_pt_pop <- mun_total |>
  left_join(mun_bus |> rename(pop_pt_bus = pop_served), by = "municipio") |>
  left_join(mun_metrolr |> rename(pop_pt_metrolr = pop_served), by = "municipio") |>
  left_join(mun_trainferry |> rename(pop_pt_trainferry = pop_served), by = "municipio") |>
  left_join(mun_all |> rename(pop_pt_all = pop_served), by = "municipio") |>
  mutate(
    across(starts_with("pop_pt_"), ~ replace_na(.x, 0)),
    across(starts_with("pop_pt_"), round),
    pct_pt_bus        = round(pop_pt_bus / pop_total, 4),
    pct_pt_metrolr    = round(pop_pt_metrolr / pop_total, 4),
    pct_pt_trainferry = round(pop_pt_trainferry / pop_total, 4),
    pct_pt_all        = round(pop_pt_all / pop_total, 4)
  )

# mapview(municipios |> left_join(municipios_pt_pop, by = "municipio"), zcol = "pct_pt_all")


# 10. Export -----------------------------------------------------------------

# Grid: spatial + tabular
impt_write(
  grid_pt_pop |>
    select(id, population, starts_with("pt_served_"), starts_with("pop_pt_"), starts_with("pct_pt_")),
  "/mobility/grid_pop_pt_served.gpkg"
)
impt_write(
  grid_pt_pop |>
    st_drop_geometry() |>
    select(id, population, starts_with("pt_served_"), starts_with("pop_pt_"), starts_with("pct_pt_")),
  "/mobility/grid_pop_pt_served.csv"
)

# Freguesia
impt_write(freguesias_pt_pop, "/mobility/freguesias_pop_pt_served.csv")

# Município
impt_write(municipios_pt_pop, "/mobility/municipios_pop_pt_served.csv")

message("Done! Outputs written to /mobility/")
