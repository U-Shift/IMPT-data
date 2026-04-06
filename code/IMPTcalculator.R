# =============================================================================
# IMPcalculator.R
# IMPT - Integrated Mobility Performance Tool
# Computes IMPT scores at the freguesia, municipality, and grid levels.
#
# Dimensions:  Accessibility | Mobility | Affordability | Safety
# Transport modes:  Car | PT | Bike | Walk
# Weighting methods:  PCA (PC1 scores) | Entropy weights
#
# NOTE: Input data live under /data/IMPT/ on the remote server.
# =============================================================================


# ── 0. Libraries ──────────────────────────────────────────────────────────────

library(dplyr)
library(readr)
library(stringr)
library(FactoMineR)
library(sf)
library(mapview)


# =============================================================================
# SECTION 1 – DATA LOADING
# =============================================================================

# ── 1.1 Accessibility ─────────────────────────────────────────────────────────

Accessibility <- read_csv("/data/IMPT/results_data/freguesia_accessibility.csv") |>
  select(-nuts)


# ── 1.2 Mobility ──────────────────────────────────────────────────────────────

# Infrastructure ratios
freguesias_infrastructure_ratio <- read_csv("/data/IMPT/mobility/freguesias_infrastructure_ratio.csv") |>
  select(
    dtmnfr, road_length, pedpath_length, cycleway_length,
    segregated_cycleway_length, pedpath_to_road_ratio,
    cycleway_to_road_ratio, cycling_quality_ratio
  )

# Stops coverage — requires a name-to-code lookup from the GeoPackage
freguesias_names <- st_read("/data/IMPT/geo/freguesias_2024_unique.gpkg") |>
  st_drop_geometry() |>
  select(dtmnfr, freguesia)

freguesias_stops_coverage <- read_csv("/data/IMPT/mobility/freguesias_stops_coverage.csv") |>
  select(freguesia, ratio_served_population) |>
  left_join(freguesias_names, by = "freguesia") |>
  select(-freguesia) |>
  mutate(dtmnfr = as.numeric(dtmnfr)) |>
  rename(Stops_coverage = ratio_served_population)

# Shared mobility
freguesias_shared_mobility <- read_csv("/data/IMPT/mobility/freguesias_shared_mobility.csv") |>
  select(dtmnfr, shared_mobility_points) |>
  rename(Shared_mobility = shared_mobility_points)

# Pre-aggregated mobility results
freguesias_mobility <- read_csv("/data/IMPT/results_data/freguesia_mobility.csv") |>
  select(-nuts)

# Transit headways
freguesias_headways <- read_csv("/data/IMPT/mobility_transit/freguesias_headways.csv") |>
  select(dtmnfr, weighted_frequency_peak, weighted_waiting_time_peak) |>
  rename(
    PT_Weight_frequency_peak = weighted_frequency_peak,
    PT_Waitingtime           = weighted_waiting_time_peak
  )

# Join all mobility tables
Mobility <- freguesias_infrastructure_ratio |>
  left_join(freguesias_stops_coverage, by = "dtmnfr") |>
  left_join(freguesias_shared_mobility, by = "dtmnfr") |>
  left_join(freguesias_mobility, by = "dtmnfr") |>
  left_join(freguesias_headways, by = "dtmnfr")


# ── 1.3 Safety ────────────────────────────────────────────────────────────────

Safety <- read_csv("/data/IMPT/safety/accidents_by_freguesia_5years_dentrolocalidades.csv") |>
  select(-freguesia, -mun_id, -population) |>
  rename(dtmnfr = freg_id) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x))) # NAs → 0


# ── 1.4 Affordability ─────────────────────────────────────────────────────────

freguesia_affordability_mob <- read_csv("/data/IMPT/results_data/freguesia_affordability.csv")

freguesia_income <- read_csv("/data/IMPT/landuse/freguesias_income_housing_gini.csv") |>
  select(-gini_coef) |>
  rename(dtmnfr = freg_id) |>
  mutate(housing_costs_year = housing_costs * 12)

freguesia_affordability <- freguesia_affordability_mob |>
  left_join(freguesia_income, by = "dtmnfr")

# NAVEGANTE monthly pass scenario
Affordability_navegante <- freguesia_affordability |>
  mutate(
    h_transp_inc_car = (housing_costs_year + affordability_car_total_money * 250) / income_hh,
    h_transp_inc_pt  = (housing_costs_year + affordability_transit_pass_total_money * 250) / income_hh,
    transp_inc_car   = (affordability_car_total_money * 250) / income_hh,
    transp_inc_pt    = (affordability_transit_pass_total_money * 250) / income_hh
  ) |>
  select(dtmnfr, h_transp_inc_car, h_transp_inc_pt, transp_inc_car, transp_inc_pt)

# Impute Canha (dtmnfr == "150701") with the mean of its neighbours in the same municipality
mean_viz_canha <- Affordability_navegante |>
  filter(str_starts(dtmnfr, "1507"), dtmnfr != "150701") |>
  summarise(
    mean_h_transp_inc_pt = mean(h_transp_inc_pt, na.rm = TRUE),
    mean_transp_inc_pt   = mean(transp_inc_pt, na.rm = TRUE)
  )

Affordability_navegante <- Affordability_navegante |>
  mutate(
    h_transp_inc_pt = case_when(dtmnfr == "150701" ~ mean_viz_canha$mean_h_transp_inc_pt, TRUE ~ h_transp_inc_pt),
    transp_inc_pt   = case_when(dtmnfr == "150701" ~ mean_viz_canha$mean_transp_inc_pt, TRUE ~ transp_inc_pt)
  )

# SINGLE FARE scenario
Affordability_singlefare <- freguesia_affordability |>
  mutate(
    h_transp_inc_car = (housing_costs_year + affordability_car_total_money * 250) / income_hh,
    h_transp_inc_pt  = (housing_costs_year + affordability_transit_single_fare_total_money * 250) / income_hh,
    transp_inc_car   = (affordability_car_total_money * 250) / income_hh,
    transp_inc_pt    = (affordability_transit_single_fare_total_money * 250) / income_hh
  ) |>
  select(dtmnfr, h_transp_inc_car, h_transp_inc_pt, transp_inc_car, transp_inc_pt)

mean_viz_canha2 <- Affordability_singlefare |>
  filter(str_starts(dtmnfr, "1507"), dtmnfr != "150701") |>
  summarise(
    mean_h_transp_inc_pt = mean(h_transp_inc_pt, na.rm = TRUE),
    mean_transp_inc_pt   = mean(transp_inc_pt, na.rm = TRUE)
  )

Affordability_singlefare <- Affordability_singlefare |>
  mutate(
    h_transp_inc_pt = case_when(dtmnfr == "150701" ~ mean_viz_canha2$mean_h_transp_inc_pt, TRUE ~ h_transp_inc_pt),
    transp_inc_pt   = case_when(dtmnfr == "150701" ~ mean_viz_canha2$mean_transp_inc_pt, TRUE ~ transp_inc_pt)
  )

# Census modal share (used to compute modal-share-weighted affordability)
census_modal_share <- read_csv("/data/IMPT/census2021/census_modal_share_parish.csv") |>
  select(dtmnfr, total, pt, private_vehicle, active, pt_share, private_vehicle_share, active_share)


# =============================================================================
# SECTION 2 – SPLIT DATA BY MODE
# =============================================================================

# Accessibility
Accessibility_car <- Accessibility |> select(dtmnfr, contains("_car_"))
Accessibility_pt <- Accessibility |> select(dtmnfr, contains("_transit_"))
Accessibility_bike <- Accessibility |> select(dtmnfr, contains("_bike_"))
Accessibility_walk <- Accessibility |> select(dtmnfr, contains("_walk_"))

# Mobility
Mobility_car <- Mobility |> select(dtmnfr, contains("_car"), road_length)
Mobility_pt <- Mobility |> select(dtmnfr, contains("PT"), Stops_coverage, contains("transit"), Shared_mobility)
Mobility_bike <- Mobility |> select(dtmnfr, contains("cycl"), contains("bike"))
Mobility_walk <- Mobility |> select(dtmnfr, contains("_walk"), contains("ped"))

# Affordability (split from the scenario-level tables)
Affordability_car <- Affordability_navegante |> select(dtmnfr, contains("car"))
Affordability_pt_navegante <- Affordability_navegante |> select(dtmnfr, contains("pt"))
Affordability_pt_singlefare <- Affordability_singlefare |> select(dtmnfr, contains("pt"))

# Safety
Safety_car <- Safety |> select(dtmnfr, contains("car"), contains("motorizado"))
Safety_bike <- Safety |>
  select(dtmnfr, contains("bici")) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)))
Safety_walk <- Safety |> select(dtmnfr, contains("peo"))


# =============================================================================
# SECTION 3 – NORMALISATION
# =============================================================================

#' Normalize a vector so that higher raw value → higher score (benefit indicator).
#' Result is scaled to [1, 100].
normalize_benefit <- function(x) {
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  1 + ((x - x_min) / (x_max - x_min)) * 99
}

#' Normalize a vector so that lower raw value → higher score (cost indicator).
#' Result is scaled to [1, 100].
normalize_cost <- function(x) {
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  1 + ((x_max - x) / (x_max - x_min)) * 99
}

# ── 3.1 Global (all-mode) normalised datasets ─────────────────────────────────

Accessibility_Norm <- Accessibility |>
  mutate(
    across(starts_with("access"), ~ normalize_benefit(.x)),
    across(starts_with("mobility"), ~ normalize_cost(.x))
  ) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

Mobility_Norm <- Mobility |>
  mutate(
    across(starts_with(c("PT_Wait", "mobility")), ~ normalize_cost(.x)),
    across(!c(dtmnfr, starts_with(c("PT_Wait", "mobility"))), ~ normalize_benefit(.x))
  )

Affordability_Norm_navegante <- Affordability_navegante |>
  mutate(across(!dtmnfr, ~ normalize_cost(.x)))

Affordability_Norm_singlefare <- Affordability_singlefare |>
  mutate(across(!dtmnfr, ~ normalize_cost(.x)))

Safety_Norm <- Safety |>
  mutate(across(!dtmnfr, ~ normalize_cost(.x)))


# ── 3.2 Per-mode normalised datasets ──────────────────────────────────────────

Accessibility_car_Norm <- Accessibility_car |>
  mutate(
    across(starts_with("access"), ~ normalize_benefit(.x)),
    across(starts_with("mobility"), ~ normalize_cost(.x))
  )

Accessibility_pt_Norm <- Accessibility_pt |>
  mutate(
    across(starts_with("access"), ~ normalize_benefit(.x)),
    across(starts_with("mobility"), ~ normalize_cost(.x))
  ) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

Accessibility_bike_Norm <- Accessibility_bike |>
  mutate(
    across(starts_with("access"), ~ normalize_benefit(.x)),
    across(starts_with("mobility"), ~ normalize_cost(.x))
  )

Accessibility_walk_Norm <- Accessibility_walk |>
  mutate(
    across(starts_with("access"), ~ normalize_benefit(.x)),
    across(starts_with("mobility"), ~ normalize_cost(.x))
  )


Mobility_car_Norm <- Mobility_car |>
  mutate(
    across(starts_with("mobility"), ~ normalize_cost(.x)),
    across(!c(dtmnfr, starts_with("mobility")), ~ normalize_benefit(.x))
  )

Mobility_pt_Norm <- Mobility_pt |>
  mutate(
    across(starts_with(c("PT_Wait", "mobility")), ~ normalize_cost(.x)),
    across(!c(dtmnfr, starts_with(c("PT_Wait", "mobility"))), ~ normalize_benefit(.x))
  )

Mobility_bike_Norm <- Mobility_bike |>
  mutate(
    across(starts_with("mobility"), ~ normalize_cost(.x)),
    across(!c(dtmnfr, starts_with("mobility")), ~ normalize_benefit(.x))
  )

Mobility_walk_Norm <- Mobility_walk |>
  mutate(
    across(starts_with("mobility"), ~ normalize_cost(.x)),
    across(!c(dtmnfr, starts_with("mobility")), ~ normalize_benefit(.x))
  )


Affordability_car_Norm <- Affordability_car |> mutate(across(!dtmnfr, ~ normalize_cost(.x)))
Affordability_pt_Norm_navegante <- Affordability_pt_navegante |> mutate(across(!dtmnfr, ~ normalize_cost(.x)))
Affordability_pt_Norm_singlefare <- Affordability_pt_singlefare |> mutate(across(!dtmnfr, ~ normalize_cost(.x)))

Safety_car_Norm <- Safety_car |> mutate(across(!dtmnfr, ~ normalize_cost(.x)))
Safety_bike_Norm <- Safety_bike |> mutate(across(!dtmnfr, ~ normalize_cost(.x)))
Safety_walk_Norm <- Safety_walk |> mutate(across(!dtmnfr, ~ normalize_cost(.x)))


# ── 3.3 Modal-share-weighted affordability ────────────────────────────────────

# Weights each mode's affordability score by its observed modal share

afford_navegante <- Affordability_Norm_navegante |>
  left_join(census_modal_share, by = "dtmnfr") |>
  mutate(
    MS_CAR_AFF      = h_transp_inc_car * private_vehicle_share,
    MS_PT_AFF       = h_transp_inc_pt * pt_share,
    MS_AFF_POST_NAV = (MS_CAR_AFF + MS_PT_AFF) / (private_vehicle_share + pt_share + active_share)
  )

afford_singlefare <- Affordability_Norm_singlefare |>
  left_join(census_modal_share, by = "dtmnfr") |>
  mutate(
    MS_CAR_AFF      = h_transp_inc_car * private_vehicle_share,
    MS_PT_AFF       = h_transp_inc_pt * pt_share,
    MS_AFF_PRE_NAV  = (MS_CAR_AFF + MS_PT_AFF) / (private_vehicle_share + pt_share + active_share)
  )


# ── 3.4 Export normalised datasets ────────────────────────────────────────────

write_csv(Mobility_Norm, "/data/IMPT/impt/norm/Mobility_Norm.csv")
write_csv(Accessibility_Norm, "/data/IMPT/impt/norm/Accessibility_Norm.csv")
write_csv(Affordability_Norm_navegante, "/data/IMPT/impt/norm/Affordability_Norm_navegante.csv")
write_csv(Affordability_Norm_singlefare, "/data/IMPT/impt/norm/Affordability_Norm_singlefare.csv")
write_csv(Safety_Norm, "/data/IMPT/impt/norm/Safety_Norm.csv")

write_csv(Mobility_car_Norm, "/data/IMPT/impt/norm/Mobility_car_Norm.csv")
write_csv(Mobility_pt_Norm, "/data/IMPT/impt/norm/Mobility_pt_Norm.csv")
write_csv(Mobility_bike_Norm, "/data/IMPT/impt/norm/Mobility_bike_Norm.csv")
write_csv(Mobility_walk_Norm, "/data/IMPT/impt/norm/Mobility_walk_Norm.csv")

write_csv(Accessibility_car_Norm, "/data/IMPT/impt/norm/Accessibility_car_Norm.csv")
write_csv(Accessibility_pt_Norm, "/data/IMPT/impt/norm/Accessibility_pt_Norm.csv")
write_csv(Accessibility_bike_Norm, "/data/IMPT/impt/norm/Accessibility_bike_Norm.csv")
write_csv(Accessibility_walk_Norm, "/data/IMPT/impt/norm/Accessibility_walk_Norm.csv")

write_csv(Affordability_car_Norm, "/data/IMPT/impt/norm/Affordability_car_Norm.csv")
write_csv(Affordability_pt_Norm_singlefare, "/data/IMPT/impt/norm/Affordability_pt_Norm_singlefare.csv")
write_csv(Affordability_pt_Norm_navegante, "/data/IMPT/impt/norm/Affordability_pt_Norm_navegante.csv")

write_csv(Safety_car_Norm, "/data/IMPT/impt/norm/Safety_car_Norm.csv")
write_csv(Safety_bike_Norm, "/data/IMPT/impt/norm/Safety_bike_Norm.csv")
write_csv(Safety_walk_Norm, "/data/IMPT/impt/norm/Safety_walk_Norm.csv")


# =============================================================================
# SECTION 4 – PCA-BASED DIMENSION SCORES
# =============================================================================

# Helper: rescale any numeric vector to [1, 100]
scale_0_100 <- function(x) {
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  1 + ((x - x_min) / (x_max - x_min)) * 99
}

# Helper: show the top-n indicator contributions to PC1
champions <- function(pca_obj, dim = 1, n = 10) {
  contributions <- pca_obj$var$contrib[, dim]
  sort(contributions, decreasing = TRUE) |> head(n)
}


# ── 4.1 Global PCA ────────────────────────────────────────────────────────────

pca_access <- PCA(Accessibility_Norm, quali.sup = 1, graph = FALSE)
pca_mobility <- PCA(Mobility_Norm, quali.sup = 1, graph = FALSE)
pca_safety <- PCA(Safety_Norm, quali.sup = 1, graph = FALSE)

# Extract global dimension scores (PC1, rescaled to [1, 100])
acc_scores <- data.frame(
  dtmnfr = Accessibility_Norm$dtmnfr,
  Accessibility_Index = scale_0_100(pca_access$ind$coord[, 1])
)

mob_scores <- data.frame(
  dtmnfr         = Mobility_Norm$dtmnfr,
  Mobility_Index = scale_0_100(pca_mobility$ind$coord[, 1])
)

saf_scores <- data.frame(
  dtmnfr       = Safety_Norm$dtmnfr,
  Safety_Index = scale_0_100(pca_safety$ind$coord[, 1])
)

# Affordability: modal-share-weighted composite (no PCA — too few indicators)
afford_navegante_fin <- afford_navegante |>
  select(dtmnfr, MS_AFF_POST_NAV) |>
  rename(Affordability_Index_navegante = MS_AFF_POST_NAV)

afford_singlefare_fin <- afford_singlefare |>
  select(dtmnfr, MS_AFF_PRE_NAV) |>
  rename(Affordability_Index_singlefare = MS_AFF_PRE_NAV)

# Champions (top contributing indicators per dimension)
Champions_access <- champions(pca_access)
Champions_mobility <- champions(pca_mobility)
Champions_safety <- champions(pca_safety)


# ── 4.2 Per-mode PCA ──────────────────────────────────────────────────────────

pca_car_access <- PCA(Accessibility_car_Norm, quali.sup = 1, graph = FALSE)
pca_pt_access <- PCA(Accessibility_pt_Norm, quali.sup = 1, graph = FALSE)
pca_bike_access <- PCA(Accessibility_bike_Norm, quali.sup = 1, graph = FALSE)
pca_walk_access <- PCA(Accessibility_walk_Norm, quali.sup = 1, graph = FALSE)

pca_car_mobility <- PCA(Mobility_car_Norm, quali.sup = 1, graph = FALSE)
pca_pt_mobility <- PCA(Mobility_pt_Norm, quali.sup = 1, graph = FALSE)
pca_bike_mobility <- PCA(Mobility_bike_Norm, quali.sup = 1, graph = FALSE)
pca_walk_mobility <- PCA(Mobility_walk_Norm, quali.sup = 1, graph = FALSE)

pca_car_safety <- PCA(Safety_car_Norm, quali.sup = 1, graph = FALSE)
pca_bike_safety <- PCA(Safety_bike_Norm, quali.sup = 1, graph = FALSE)
pca_walk_safety <- PCA(Safety_walk_Norm, quali.sup = 1, graph = FALSE)

# Per-mode accessibility scores
acc_car_scores <- data.frame(
  dtmnfr = Accessibility_car_Norm$dtmnfr,
  Accessibility_Index = scale_0_100(as.numeric(pca_car_access$ind$coord[, 1]))
)
acc_pt_scores <- data.frame(
  dtmnfr = Accessibility_pt_Norm$dtmnfr,
  Accessibility_Index = scale_0_100(as.numeric(pca_pt_access$ind$coord[, 1]))
)
acc_bike_scores <- data.frame(
  dtmnfr = Accessibility_bike_Norm$dtmnfr,
  Accessibility_Index = scale_0_100(as.numeric(pca_bike_access$ind$coord[, 1]))
)
acc_walk_scores <- data.frame(
  dtmnfr = Accessibility_walk_Norm$dtmnfr,
  Accessibility_Index = scale_0_100(as.numeric(pca_walk_access$ind$coord[, 1]))
)

# Per-mode affordability scores (direct normalised values — PCA not applicable)
aff_car_scores <- Affordability_car_Norm |>
  select(dtmnfr, h_transp_inc_car) |>
  rename(Affordability_Index = h_transp_inc_car)
aff_pt_scores_navegante <- Affordability_pt_Norm_navegante |>
  select(dtmnfr, h_transp_inc_pt) |>
  rename(Affordability_Index_navegante = h_transp_inc_pt)
aff_pt_scores_singlefare <- Affordability_pt_Norm_singlefare |>
  select(dtmnfr, h_transp_inc_pt) |>
  rename(Affordability_Index_singlefare = h_transp_inc_pt)

# Per-mode mobility scores
mob_car_scores <- data.frame(
  dtmnfr = Mobility_car_Norm$dtmnfr,
  Mobility_Index = scale_0_100(pca_car_mobility$ind$coord[, 1])
)
mob_pt_scores <- data.frame(
  dtmnfr = Mobility_pt_Norm$dtmnfr,
  Mobility_Index = scale_0_100(pca_pt_mobility$ind$coord[, 1])
)
mob_bike_scores <- data.frame(
  dtmnfr = Mobility_bike_Norm$dtmnfr,
  Mobility_Index = scale_0_100(pca_bike_mobility$ind$coord[, 1])
)
mob_walk_scores <- data.frame(
  dtmnfr = Mobility_walk_Norm$dtmnfr,
  Mobility_Index = scale_0_100(pca_walk_mobility$ind$coord[, 1])
)

# Per-mode safety scores (equal-weight average of severity index + frequency)
# NOTE: severity index is inverted because it is already normalised as "cost"
saf_car_scores <- Safety_car_Norm |>
  mutate(
    indice_gravidade_carro = 100 - indice_gravidade_carro,
    Safety_Index = indice_gravidade_carro * 0.5 + veh_motorizado * 0.5
  ) |>
  select(dtmnfr, Safety_Index)

saf_bike_scores <- Safety_bike_Norm |>
  mutate(
    indice_gravidade_bicicleta = 100 - indice_gravidade_bicicleta,
    Safety_Index = indice_gravidade_bicicleta * 0.5 + veh_bicicleta * 0.5
  ) |>
  select(dtmnfr, Safety_Index)

saf_walk_scores <- Safety_walk_Norm |>
  mutate(
    indice_gravidade_peoes = 100 - indice_gravidade_peoes,
    Safety_Index = indice_gravidade_peoes * 0.5 + veh_peoes * 0.5
  ) |>
  select(dtmnfr, Safety_Index)


# ── 4.3 PCA diagnostics ───────────────────────────────────────────────────────

pca_list <- list(
  Accessibility_GB   = pca_access,
  Mobility_GB        = pca_mobility,
  Safety_GB          = pca_safety,
  Accessibility_car  = pca_car_access,
  Accessibility_pt   = pca_pt_access,
  Accessibility_bike = pca_bike_access,
  Accessibility_walk = pca_walk_access,
  Mobility_car       = pca_car_mobility,
  Mobility_pt        = pca_pt_mobility,
  Mobility_bike      = pca_bike_mobility,
  Mobility_walk      = pca_walk_mobility,
  Safety_car         = pca_car_safety,
  Safety_bike        = pca_bike_safety,
  Safety_walk        = pca_walk_safety
)

get_pca_diagnostics <- function(pca_obj, name) {
  var_pc1 <- pca_obj$eig[1, 2]
  cors <- as.data.frame(pca_obj$var$cor[, 1])
  colnames(cors) <- "Correlation_PC1"
  cors$Variable <- rownames(cors)
  cors$Dimension_Mode <- name
  cors$PC1_Variance_Percent <- var_pc1
  cors
}

pca_variance_report <- do.call(rbind, lapply(names(pca_list), function(x) {
  get_pca_diagnostics(pca_list[[x]], x)
}))[, c("Dimension_Mode", "PC1_Variance_Percent", "Variable", "Correlation_PC1")]

pca_summary_table <- data.frame(
  Dimension_Mode       = names(pca_list),
  PC1_Variance_Percent = sapply(pca_list, function(x) x$eig[1, 2]),
  Eigenvalue_PC1       = sapply(pca_list, function(x) x$eig[1, 1])
)

# Direction check: which variable is most correlated with PC1 and in which direction?
direction_results <- do.call(rbind, lapply(names(pca_list), function(x) {
  cors <- pca_list[[x]]$var$cor[, 1]
  top_idx <- which.max(abs(cors))
  data.frame(Dimension = x, Top_Variable = names(cors)[top_idx], Correlation = round(cors[top_idx], 3))
}))

print(pca_summary_table)
print(direction_results)


# ── 4.4 Export PCA scores & diagnostics ───────────────────────────────────────

write_csv(acc_scores, "/data/IMPT/impt/pca_scores/Accessibility_PCA_Scores.csv")
write_csv(saf_scores, "/data/IMPT/impt/pca_scores/Safety_PCA_Scores.csv")
write_csv(mob_scores, "/data/IMPT/impt/pca_scores/Mobility_PCA_Scores.csv")

write_csv(acc_car_scores, "/data/IMPT/impt/pca_scores/Accessibility_PCA_car_Scores.csv")
write_csv(acc_pt_scores, "/data/IMPT/impt/pca_scores/Accessibility_PCA_pt_Scores.csv")
write_csv(acc_bike_scores, "/data/IMPT/impt/pca_scores/Accessibility_PCA_bike_Scores.csv")
write_csv(acc_walk_scores, "/data/IMPT/impt/pca_scores/Accessibility_PCA_walk_Scores.csv")

write_csv(aff_car_scores, "/data/IMPT/impt/pca_scores/Affordability_car_Scores.csv")
write_csv(aff_pt_scores_singlefare, "/data/IMPT/impt/pca_scores/Affordability_pt_Scores_singlefare.csv")
write_csv(aff_pt_scores_navegante, "/data/IMPT/impt/pca_scores/Affordability_pt_Scores_navegante.csv")

write_csv(saf_car_scores, "/data/IMPT/impt/pca_scores/Safety_car_Scores.csv")
write_csv(saf_bike_scores, "/data/IMPT/impt/pca_scores/Safety_bike_Scores.csv")
write_csv(saf_walk_scores, "/data/IMPT/impt/pca_scores/Safety_walk_Scores.csv")

write_csv(mob_car_scores, "/data/IMPT/impt/pca_scores/Mobility_PCA_car_Scores.csv")
write_csv(mob_pt_scores, "/data/IMPT/impt/pca_scores/Mobility_PCA_pt_Scores.csv")
write_csv(mob_bike_scores, "/data/IMPT/impt/pca_scores/Mobility_PCA_bike_Scores.csv")
write_csv(mob_walk_scores, "/data/IMPT/impt/pca_scores/Mobility_PCA_walk_Scores.csv")

write_csv(pca_variance_report, "/data/IMPT/impt/pca_scores/PCA_Variance_and_Correlations.csv")
write_csv(pca_summary_table, "/data/IMPT/impt/pca_scores/PCA_Variance_Table.csv")

write_csv(
  data.frame(Indicator = names(Champions_access), Contribution = Champions_access),
  "/data/IMPT/impt/pca_scores/Champions_Accessibility.csv"
)
write_csv(
  data.frame(Indicator = names(Champions_mobility), Contribution = Champions_mobility),
  "/data/IMPT/impt/pca_scores/Champions_Mobility.csv"
)
write_csv(
  data.frame(Indicator = names(Champions_safety), Contribution = Champions_safety),
  "/data/IMPT/impt/pca_scores/Champions_Safety.csv"
)


# =============================================================================
# SECTION 5 – COMPOSITE IMPT SCORE
# =============================================================================

# Helper: entropy-based dimension weights
#   df: data.frame of dimension scores (one column per dimension, no ID column)
compute_entropy_weights <- function(df) {
  df_shifted <- df + 0.0001
  p_ij <- sweep(df_shifted, 2, colSums(df_shifted), "/")
  n <- nrow(df)
  k <- 1 / log(n)
  e_j <- -k * colSums(p_ij * log(p_ij + 1e-10))
  d_j <- 1 - e_j
  d_j / sum(d_j)
}


# ── 5.1 Global IMPT (all modes combined) ─────────────────────────────────────

Final_Results_Master <- acc_scores |>
  left_join(afford_singlefare_fin, by = "dtmnfr") |>
  left_join(afford_navegante_fin, by = "dtmnfr") |>
  left_join(mob_scores, by = "dtmnfr") |>
  left_join(saf_scores, by = "dtmnfr")

# Geometric mean  (+1 shift to avoid zeros; –1 to restore scale)
Final_Results_Master <- Final_Results_Master |>
  mutate(
    IMPT_score_pca_geom_navegante = ((Accessibility_Index + 1) * (Mobility_Index + 1) *
      (Affordability_Index_navegante + 1) * (Safety_Index + 1))^(1 / 4) - 1,
    IMPT_score_pca_geom_singlefare = ((Accessibility_Index + 1) * (Mobility_Index + 1) *
      (Affordability_Index_singlefare + 1) * (Safety_Index + 1))^(1 / 4) - 1,
    IMPT_score_pca_avg_navegante = (Accessibility_Index + Mobility_Index + Affordability_Index_navegante + Safety_Index) / 4,
    IMPT_score_pca_avg_singlefare = (Accessibility_Index + Mobility_Index + Affordability_Index_singlefare + Safety_Index) / 4
  )

# Entropy weights
ew_nav <- compute_entropy_weights(
  Final_Results_Master |> select(Accessibility_Index, Affordability_Index_navegante, Mobility_Index, Safety_Index)
)
ew_sf <- compute_entropy_weights(
  Final_Results_Master |> select(Accessibility_Index, Affordability_Index_singlefare, Mobility_Index, Safety_Index)
)

Final_Results_Master <- Final_Results_Master |>
  mutate(
    IMPT_entropy_pca_navegante = (
      (Accessibility_Index + 1)^ew_nav[1] *
        (Affordability_Index_navegante + 1)^ew_nav[2] *
        (Mobility_Index + 1)^ew_nav[3] *
        (Safety_Index + 1)^ew_nav[4]
    ) - 1,
    IMPT_entropy_pca_singlefare = (
      (Accessibility_Index + 1)^ew_sf[1] *
        (Affordability_Index_singlefare + 1)^ew_sf[2] *
        (Mobility_Index + 1)^ew_sf[3] *
        (Safety_Index + 1)^ew_sf[4]
    ) - 1
  )

write_csv(Final_Results_Master, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_freguesia.csv")


# ── 5.2 Per-mode IMPT ─────────────────────────────────────────────────────────

# --- Car ---
Final_Results_Master_car <- acc_car_scores |>
  left_join(aff_car_scores, by = "dtmnfr") |>
  left_join(mob_car_scores, by = "dtmnfr") |>
  left_join(saf_car_scores, by = "dtmnfr") |>
  mutate(
    IMPT_score_pca_geom_car = ((Accessibility_Index + 1) * (Mobility_Index + 1) *
      (Affordability_Index + 1) * (Safety_Index + 1))^(1 / 4) - 1,
    IMPT_score_pca_avg_car = (Accessibility_Index + Mobility_Index + Affordability_Index + Safety_Index) / 4
  )

ew_car <- compute_entropy_weights(
  Final_Results_Master_car |> select(Accessibility_Index, Affordability_Index, Mobility_Index, Safety_Index)
)
Final_Results_Master_car <- Final_Results_Master_car |>
  mutate(
    IMPT_entropy_pca_car = (
      (Accessibility_Index + 1)^ew_car[1] *
        (Affordability_Index + 1)^ew_car[2] *
        (Mobility_Index + 1)^ew_car[3] *
        (Safety_Index + 1)^ew_car[4]
    ) - 1
  )

# --- PT ---
Final_Results_Master_pt <- acc_pt_scores |>
  left_join(aff_pt_scores_navegante, by = "dtmnfr") |>
  left_join(aff_pt_scores_singlefare, by = "dtmnfr") |>
  left_join(mob_pt_scores, by = "dtmnfr") |>
  mutate(
    IMPT_score_pca_geom_pt_navegante = ((Accessibility_Index + 1) * (Mobility_Index + 1) *
      (Affordability_Index_navegante + 1))^(1 / 3) - 1,
    IMPT_score_pca_geom_pt_singlefare = ((Accessibility_Index + 1) * (Mobility_Index + 1) *
      (Affordability_Index_singlefare + 1))^(1 / 3) - 1,
    IMPT_score_pca_avg_pt_navegante = (Accessibility_Index + Mobility_Index + Affordability_Index_navegante) / 3,
    IMPT_score_pca_avg_pt_singlefare = (Accessibility_Index + Mobility_Index + Affordability_Index_singlefare) / 3
  )

ew_pt_nav <- compute_entropy_weights(
  Final_Results_Master_pt |> select(Accessibility_Index, Affordability_Index_navegante, Mobility_Index)
)
ew_pt_sf <- compute_entropy_weights(
  Final_Results_Master_pt |> select(Accessibility_Index, Affordability_Index_singlefare, Mobility_Index)
)
Final_Results_Master_pt <- Final_Results_Master_pt |>
  mutate(
    IMPT_entropy_pca_pt_navegante = (
      (Accessibility_Index + 1)^ew_pt_nav[1] *
        (Affordability_Index_navegante + 1)^ew_pt_nav[2] *
        (Mobility_Index + 1)^ew_pt_nav[3]
    ) - 1,
    IMPT_entropy_pca_pt_singlefare = (
      (Accessibility_Index + 1)^ew_pt_sf[1] *
        (Affordability_Index_singlefare + 1)^ew_pt_sf[2] *
        (Mobility_Index + 1)^ew_pt_sf[3]
    ) - 1
  )

# --- Bike ---
Final_Results_Master_bike <- acc_bike_scores |>
  left_join(mob_bike_scores, by = "dtmnfr") |>
  left_join(saf_bike_scores, by = "dtmnfr") |>
  mutate(
    IMPT_score_pca_geom_bike = ((Accessibility_Index + 1) * (Mobility_Index + 1) * (Safety_Index + 1))^(1 / 3) - 1,
    IMPT_score_pca_avg_bike  = (Accessibility_Index + Mobility_Index + Safety_Index) / 3
  )

ew_bike <- compute_entropy_weights(
  Final_Results_Master_bike |> select(Accessibility_Index, Mobility_Index, Safety_Index)
)
Final_Results_Master_bike <- Final_Results_Master_bike |>
  mutate(
    IMPT_entropy_pca_bike = (
      (Accessibility_Index + 1)^ew_bike[1] *
        (Mobility_Index + 1)^ew_bike[2] *
        (Safety_Index + 1)^ew_bike[3]
    ) - 1
  )

# --- Walk ---
Final_Results_Master_walk <- acc_walk_scores |>
  left_join(mob_walk_scores, by = "dtmnfr") |>
  left_join(saf_walk_scores, by = "dtmnfr") |>
  mutate(
    IMPT_score_pca_geom_walk = ((Accessibility_Index + 1) * (Mobility_Index + 1) * (Safety_Index + 1))^(1 / 3) - 1,
    IMPT_score_pca_avg_walk  = (Accessibility_Index + Mobility_Index + Safety_Index) / 3
  )

ew_walk <- compute_entropy_weights(
  Final_Results_Master_walk |> select(Accessibility_Index, Mobility_Index, Safety_Index)
)
Final_Results_Master_walk <- Final_Results_Master_walk |>
  mutate(
    IMPT_entropy_pca_walk = (
      (Accessibility_Index + 1)^ew_walk[1] *
        (Mobility_Index + 1)^ew_walk[2] *
        (Safety_Index + 1)^ew_walk[3]
    ) - 1
  )

write_csv(Final_Results_Master_car, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_car_freguesia.csv")
write_csv(Final_Results_Master_pt, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_pt_freguesia.csv")
write_csv(Final_Results_Master_bike, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_bike_freguesia.csv")
write_csv(Final_Results_Master_walk, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_walk_freguesia.csv")


# =============================================================================
# SECTION 6 – SPATIAL AGGREGATION
# =============================================================================

# Population lookup: used to compute population-weighted means at higher levels
census24_fregmun_pop <- read_csv("useful_data/census24_fregmun.csv") |>
  select(freg_id, mun_id, population) |>
  mutate(dtmnfr = as.integer(freg_id))

# Grid lookup: assigns each grid cell to a freguesia (generalised assumption)
grid_freg_mun <- read.csv("useful_data/grid_nuts.csv") |>
  select(grid_id, freg_id) |>
  mutate(freg_id = as.integer(freg_id))


# Helper: aggregate from freguesia to a higher spatial unit using pop-weighted mean
aggregate_to_level <- function(df, lookup, by_col) {
  df |>
    left_join(lookup, by = "dtmnfr") |>
    group_by(across(all_of(by_col))) |>
    summarize_at(vars(-dtmnfr, -freg_id, -population), ~ weighted.mean(., w = population, na.rm = TRUE))
}


# ── 6.1 Municipality level ────────────────────────────────────────────────────

Final_Results_Master_municipio <- aggregate_to_level(Final_Results_Master, census24_fregmun_pop, "mun_id")
Final_Results_Master_car_municipio <- aggregate_to_level(Final_Results_Master_car, census24_fregmun_pop, "mun_id")
Final_Results_Master_pt_municipio <- aggregate_to_level(Final_Results_Master_pt, census24_fregmun_pop, "mun_id")
Final_Results_Master_bike_municipio <- aggregate_to_level(Final_Results_Master_bike, census24_fregmun_pop, "mun_id")
Final_Results_Master_walk_municipio <- aggregate_to_level(Final_Results_Master_walk, census24_fregmun_pop, "mun_id")

write_csv(Final_Results_Master_municipio, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_municipio.csv")
write_csv(Final_Results_Master_car_municipio, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_car_municipio.csv")
write_csv(Final_Results_Master_pt_municipio, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_pt_municipio.csv")
write_csv(Final_Results_Master_bike_municipio, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_bike_municipio.csv")
write_csv(Final_Results_Master_walk_municipio, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_walk_municipio.csv")


# ── 6.2 Grid level ────────────────────────────────────────────────────────────

join_to_grid <- function(df) {
  grid_freg_mun |>
    left_join(df, by = c("freg_id" = "dtmnfr")) |>
    select(-freg_id)
}

Final_Results_Master_grid <- join_to_grid(Final_Results_Master)
Final_Results_Master_grid_car <- join_to_grid(Final_Results_Master_car)
Final_Results_Master_grid_pt <- join_to_grid(Final_Results_Master_pt)
Final_Results_Master_grid_bike <- join_to_grid(Final_Results_Master_bike)
Final_Results_Master_grid_walk <- join_to_grid(Final_Results_Master_walk)

write.csv(Final_Results_Master_grid, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_grid.csv", row.names = FALSE)
write.csv(Final_Results_Master_grid_car, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_car_grid.csv", row.names = FALSE)
write.csv(Final_Results_Master_grid_pt, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_pt_grid.csv", row.names = FALSE)
write.csv(Final_Results_Master_grid_bike, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_bike_grid.csv", row.names = FALSE)
write.csv(Final_Results_Master_grid_walk, "/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_walk_grid.csv", row.names = FALSE)


# =============================================================================
# SECTION 7 – VISUALISATION  (requires data_load.R to have loaded `freguesias`)
# =============================================================================

# source("code/data_load.R")   # uncomment if `freguesias` is not already in memory

impt_pca <- read.csv("/data/IMPT/impt/results/IMPT_PCA_and_Entropy_Scores_freguesia.csv")

impt_pca_sf <- freguesias |>
  select(-nuts2, -area_ha) |>
  mutate(dtmnfr = as.integer(dtmnfr)) |>
  left_join(
    impt_pca |>
      rename(
        Accessibility_PCA = Accessibility_Index,
        Affordability_PCA_prenav = Affordability_Index_singlefare,
        Affordability_PCA_postnav = Affordability_Index_navegante,
        Mobility_PCA = Mobility_Index,
        Safety_PCA = Safety_Index
      )
  ) |>
  mutate(
    across(starts_with("Accessibility"), ~ round(., 2)),
    across(starts_with("Affordability"), ~ round(., 2)),
    across(starts_with("Mobility"), ~ round(., 2)),
    across(starts_with("Safety"), ~ round(., 2)),
    across(starts_with("IMPT"), ~ round(., 2))
  ) |>
  select(
    dtmnfr, freguesia, municipio,
    starts_with("Accessibility"), starts_with("Affordability"),
    starts_with("Mobility"), starts_with("Safety"),
    starts_with("IMPT")
  )

mapview(impt_pca_sf, zcol = "Accessibility_PCA", layer.name = "PCA Accessibility", hide = TRUE) +
  mapview(impt_pca_sf, zcol = "Affordability_PCA_prenav", layer.name = "PCA Affordability – single fare", hide = TRUE) +
  mapview(impt_pca_sf, zcol = "Affordability_PCA_postnav", layer.name = "PCA Affordability – navegante", hide = TRUE) +
  mapview(impt_pca_sf, zcol = "Mobility_PCA", layer.name = "PCA Mobility", hide = TRUE) +
  mapview(impt_pca_sf, zcol = "Safety_PCA", layer.name = "PCA Safety", hide = TRUE) +
  mapview(impt_pca_sf, zcol = "IMPT_score_pca_geom_navegante", layer.name = "IMPT PCA Geom – navegante", hide = TRUE) +
  mapview(impt_pca_sf, zcol = "IMPT_score_pca_geom_singlefare", layer.name = "IMPT PCA Geom – single fare", hide = TRUE) +
  mapview(impt_pca_sf, zcol = "IMPT_score_pca_avg_navegante", layer.name = "IMPT PCA Avg – navegante", hide = TRUE) +
  mapview(impt_pca_sf, zcol = "IMPT_score_pca_avg_singlefare", layer.name = "IMPT PCA Avg – single fare", hide = TRUE) +
  mapview(impt_pca_sf, zcol = "IMPT_entropy_pca_navegante", layer.name = "IMPT Entropy – navegante", hide = TRUE) +
  mapview(impt_pca_sf, zcol = "IMPT_entropy_pca_singlefare", layer.name = "IMPT Entropy – single fare", hide = TRUE)
