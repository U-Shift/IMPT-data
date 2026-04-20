# =============================================================================
# 05_IMPTcalculator.R
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

Accessibility <- impt_read("/accessibility/r8/accessibility_freguesia.csv") |>
  left_join(impt_read("/mobility_costs/r8/mobility_freguesia.csv"), by = "dtmnfr") |>
  select(
    dtmnfr,
    # Number of opportunities reachable within threshold (travel time)
    # - Healthcare (15 Walk-Cycle; 30 min PT-CAR)
    access_health_walk_15min_residents, access_health_bike_15min_residents, access_health_transit_2t_30min_residents, access_health_car_30min_residents,
    # - Basic Education (15 Walk-Cycle; 30 min PT-CAR)
    access_schools_primary_bike_15min_kids, access_schools_primary_walk_15min_kids, access_schools_primary_transit_2t_30min_kids, access_schools_primary_car_30min_kids,
    # - Green Spaces (15-30 min Walk-Cycle; 5-10 min PT-CAR)
    access_greenspaces_walk_15min_residents, access_greenspaces_bike_15min_residents, access_greenspaces_transit_2t_5min_residents, access_greenspaces_car_5min_residents,
    # - Recreation (15-30 min Walk-Cycle; 5-10 min PT-CAR)
    access_recreation_walk_15min_residents, access_recreation_bike_15min_residents, access_recreation_transit_2t_5min_residents, access_recreation_car_5min_residents,
    # - Groceries and basic shopping (15 min all modes)
    access_groceries_walk_15min_residents, access_groceries_bike_15min_residents, access_groceries_transit_2t_15min_residents, access_groceries_car_15min_residents,
    # - Jobs (30-45 min all modes)
    access_jobs_walk_30min_active, access_jobs_bike_30min_active, access_jobs_transit_2t_45min_active, access_jobs_car_45min_active,

    # Travel time to first n opportunities
    # - Healthcare (1st opportunity)
    mobility_cost_health_walk_n1_residents, mobility_cost_health_bike_n1_residents, mobility_cost_health_transit_2t_n1_residents, mobility_cost_health_car_n1_residents,
    # - Basic Education (1st opportunity)
    mobility_cost_schools_primary_walk_n1_kids, mobility_cost_schools_primary_bike_n1_kids, mobility_cost_schools_primary_transit_2t_n1_kids, mobility_cost_schools_primary_car_n1_kids,
    # - Green Spaces (1st opportunity)
    mobility_cost_greenspaces_walk_n1_residents, mobility_cost_greenspaces_bike_n1_residents, mobility_cost_greenspaces_transit_2t_n1_residents, mobility_cost_greenspaces_car_n1_residents,
    # - Recreation (3 opportinities)
    mobility_cost_recreation_walk_n3_residents, mobility_cost_recreation_bike_n3_residents, mobility_cost_recreation_transit_2t_n3_residents, mobility_cost_recreation_car_n3_residents,
    # - Groceries and basic shopping (3 opportunities)
    mobility_cost_groceries_walk_n3_residents, mobility_cost_groceries_bike_n3_residents, mobility_cost_groceries_transit_2t_n3_residents, mobility_cost_groceries_car_n3_residents,
    # - Jobs (3 opportunities)
    mobility_cost_jobs_walk_n3_active, mobility_cost_jobs_bike_n3_active, mobility_cost_jobs_transit_2t_n3_active, mobility_cost_jobs_car_n3_active
  )


# ── 1.2 Mobility ──────────────────────────────────────────────────────────────

# Infrastructure ratios
freguesias_infrastructure_ratio <- impt_read("/mobility/freguesias_infrastructure_ratio.csv") |>
  select(
    dtmnfr, road_length, pedpath_length, cycleway_length,
    segregated_cycleway_length, pedpath_to_road_ratio,
    cycleway_to_road_ratio, cycling_quality_ratio
  )

# Stops coverage
freguesias_stops_coverage <- impt_read("/mobility/freguesias_pop_stops_coverage.csv") |>
  select(dtmnfr, pct_pt_all) |>
  mutate(dtmnfr = as.numeric(dtmnfr))

# Shared mobility
freguesias_shared_mobility <- impt_read("/mobility/freguesias_shared_mobility.csv") |>
  select(dtmnfr, shared_mobility_points) |>
  rename(Shared_mobility = shared_mobility_points)

# Pre-aggregated mobility results
freguesias_mobility <- impt_read("/mobility_commuting/freguesia_commuting.csv") |>
  rename(id = Origin_dicofre24) |>
  mutate(id = as.character(id)) |>
  rename_with(~ paste0("mobility_commuting_", .), -id) |>
  left_join(
    impt_read("/mobility_transit/freguesias_headways.csv") |>
      rename(id = dtmnfr) |>
      mutate(id = as.character(id)) |>
      rename_with(~ paste0("mobility_transit_", .), -id)
  ) |>
  left_join(
    impt_read("/mobility_commuting/freguesia_transfers.csv") |>
      rename(id = Origin_dicofre24) |>
      mutate(id = as.character(id)) |>
      select(id, weighted_mean_transfers) |>
      rename(mobility_transit_commuting_mean_transfers = weighted_mean_transfers)
  ) |>
  select(
    id,
    # Travel time (peak)
    mobility_commuting_avg_tt_bike, mobility_commuting_avg_tt_walk, mobility_commuting_avg_tt_transit_2t_120m_15w, mobility_commuting_avg_tt_car,
    # Number of transfers for key destinations (transit)
    mobility_transit_commuting_mean_transfers,
    # PT Waiting Times
    mobility_transit_weighted_waiting_time_peak,
    # Night/weekend service availability
    mobility_transit_weighted_frequency_reduction_night, mobility_transit_weighted_frequency_reduction_weekend
  ) |>
  mutate(dtmnfr = as.integer(id)) |>
  select(-id)

# Transit headways
freguesias_headways <- impt_read("/mobility_transit/freguesias_headways.csv") |>
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

Safety <- impt_read("/safety/accidents_by_freguesia_5years_dentrolocalidades.csv") |>
  select(-freguesia, -mun_id, -population, -noite) |>
  mutate(
    veh_motorizado_per = veh_motorizado / total_veiculos,
    veh_bicicleta_per = veh_bicicleta / total_veiculos,
    veh_peoes_per = veh_peoes / total_veiculos
  ) |>
  # select(-total_veiculos, -veh_motorizado, -veh_bicicleta, -veh_peoes) |>
  rename(dtmnfr = freg_id) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x))) # NAs → 0


# ── 1.4 Affordability ─────────────────────────────────────────────────────────

# Note: The computation of the raw affordability metrics has been moved out to 04_affordability.R.
# We now simply read the output from that script to assemble the scenario tables.

Affordability_composite <- impt_read("/affordability/affordability_freguesia_composite.csv")

Affordability_navegante <- Affordability_composite |>
  select(
    dtmnfr,
    h_transp_inc_car,
    h_transp_inc_pt   = h_transp_inc_pt_nav,
    transp_inc_car,
    transp_inc_pt     = transp_inc_pt_nav,
    h_transp_inc_comp = h_transp_inc_comp_nav,
    transp_inc_comp   = transp_inc_comp_nav
  )

Affordability_singlefare <- Affordability_composite |>
  select(
    dtmnfr,
    h_transp_inc_car,
    h_transp_inc_pt   = h_transp_inc_pt_sf,
    transp_inc_car,
    transp_inc_pt     = transp_inc_pt_sf,
    h_transp_inc_comp = h_transp_inc_comp_sf,
    transp_inc_comp   = transp_inc_comp_sf
  )


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
Mobility_pt <- Mobility |> select(dtmnfr, contains("PT"), pct_pt_all, contains("transit"), Shared_mobility)
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
  select(-c("indice_gravidade_carro", "indice_gravidade_bicicleta", "indice_gravidade_peoes")) |>
  select(-c("veh_motorizado_per", "veh_bicicleta_per", "veh_peoes_per")) |>
  select(-c("veh_motorizado", "veh_bicicleta", "veh_peoes")) |>
  mutate(across(!dtmnfr, ~ normalize_cost(.x)))
# mutate(across(!dtmnfr, ~ normalize_benefit(.x))) # inverted - temporary fix


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
# Safety_car_Norm <- Safety_car |> mutate(across(!dtmnfr, ~ normalize_benefit(.x))) # inverted - temporary fix
# Safety_bike_Norm <- Safety_bike |> mutate(across(!dtmnfr, ~ normalize_benefit(.x)))
# Safety_walk_Norm <- Safety_walk |> mutate(across(!dtmnfr, ~ normalize_benefit(.x)))


# ── 3.4 Export normalised datasets ────────────────────────────────────────────

impt_write(Mobility_Norm, "/impt/norm/Mobility_Norm.csv")
impt_write(Accessibility_Norm, "/impt/norm/Accessibility_Norm.csv")
impt_write(Affordability_Norm_navegante, "/impt/norm/Affordability_Norm_navegante.csv")
impt_write(Affordability_Norm_singlefare, "/impt/norm/Affordability_Norm_singlefare.csv")
impt_write(Safety_Norm, "/impt/norm/Safety_Norm.csv")

impt_write(Mobility_car_Norm, "/impt/norm/Mobility_car_Norm.csv")
impt_write(Mobility_pt_Norm, "/impt/norm/Mobility_pt_Norm.csv")
impt_write(Mobility_bike_Norm, "/impt/norm/Mobility_bike_Norm.csv")
impt_write(Mobility_walk_Norm, "/impt/norm/Mobility_walk_Norm.csv")

impt_write(Accessibility_car_Norm, "/impt/norm/Accessibility_car_Norm.csv")
impt_write(Accessibility_pt_Norm, "/impt/norm/Accessibility_pt_Norm.csv")
impt_write(Accessibility_bike_Norm, "/impt/norm/Accessibility_bike_Norm.csv")
impt_write(Accessibility_walk_Norm, "/impt/norm/Accessibility_walk_Norm.csv")

impt_write(Affordability_car_Norm, "/impt/norm/Affordability_car_Norm.csv")
impt_write(Affordability_pt_Norm_singlefare, "/impt/norm/Affordability_pt_Norm_singlefare.csv")
impt_write(Affordability_pt_Norm_navegante, "/impt/norm/Affordability_pt_Norm_navegante.csv")

impt_write(Safety_car_Norm, "/impt/norm/Safety_car_Norm.csv")
impt_write(Safety_bike_Norm, "/impt/norm/Safety_bike_Norm.csv")
impt_write(Safety_walk_Norm, "/impt/norm/Safety_walk_Norm.csv")


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
pca_safety <- PCA(Safety_Norm, quali.sup = 1, graph = FALSE) #|> select(-total_veiculos, -veh_motorizado, -veh_bicicleta, -veh_peoes)

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
afford_navegante_fin <- Affordability_Norm_navegante |>
  select(dtmnfr, h_transp_inc_comp) |>
  rename(Affordability_Index_navegante = h_transp_inc_comp)

afford_singlefare_fin <- Affordability_Norm_singlefare |>
  select(dtmnfr, h_transp_inc_comp) |>
  rename(Affordability_Index_singlefare = h_transp_inc_comp)

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

# pca_car_safety <- PCA(Safety_car_Norm |> select(-veh_motorizado), quali.sup = 1, graph = FALSE)
# pca_bike_safety <- PCA(Safety_bike_Norm |> select(-veh_bicicleta), quali.sup = 1, graph = FALSE)
# pca_walk_safety <- PCA(Safety_walk_Norm |> select(-veh_peoes), quali.sup = 1, graph = FALSE)

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
    # indice_gravidade_carro = 100 - indice_gravidade_carro, # inverted temporary fix
    Safety_Index = indice_gravidade_carro * 0.5 + veh_motorizado_per * 0.5
  ) |>
  select(dtmnfr, Safety_Index)

saf_bike_scores <- Safety_bike_Norm |>
  mutate(
    # indice_gravidade_bicicleta = 100 - indice_gravidade_bicicleta, # inverted temporary fix
    Safety_Index = indice_gravidade_bicicleta * 0.5 + veh_bicicleta_per * 0.5
  ) |>
  select(dtmnfr, Safety_Index)

saf_walk_scores <- Safety_walk_Norm |>
  mutate(
    # indice_gravidade_peoes = 100 - indice_gravidade_peoes, # inverted temporary fix
    Safety_Index = indice_gravidade_peoes * 0.5 + veh_peoes_per * 0.5
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
  Mobility_walk      = pca_walk_mobility
  # Safety_car         = pca_car_safety,
  # Safety_bike        = pca_bike_safety,
  # Safety_walk        = pca_walk_safety
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

impt_write(acc_scores, "/impt/pca_scores/Accessibility_PCA_Scores.csv")
impt_write(saf_scores, "/impt/pca_scores/Safety_PCA_Scores.csv")
impt_write(mob_scores, "/impt/pca_scores/Mobility_PCA_Scores.csv")

impt_write(acc_car_scores, "/impt/pca_scores/Accessibility_PCA_car_Scores.csv")
impt_write(acc_pt_scores, "/impt/pca_scores/Accessibility_PCA_pt_Scores.csv")
impt_write(acc_bike_scores, "/impt/pca_scores/Accessibility_PCA_bike_Scores.csv")
impt_write(acc_walk_scores, "/impt/pca_scores/Accessibility_PCA_walk_Scores.csv")

impt_write(aff_car_scores, "/impt/pca_scores/Affordability_car_Scores.csv")
impt_write(aff_pt_scores_singlefare, "/impt/pca_scores/Affordability_pt_Scores_singlefare.csv")
impt_write(aff_pt_scores_navegante, "/impt/pca_scores/Affordability_pt_Scores_navegante.csv")

impt_write(saf_car_scores, "/impt/pca_scores/Safety_car_Scores.csv")
impt_write(saf_bike_scores, "/impt/pca_scores/Safety_bike_Scores.csv")
impt_write(saf_walk_scores, "/impt/pca_scores/Safety_walk_Scores.csv")

impt_write(mob_car_scores, "/impt/pca_scores/Mobility_PCA_car_Scores.csv")
impt_write(mob_pt_scores, "/impt/pca_scores/Mobility_PCA_pt_Scores.csv")
impt_write(mob_bike_scores, "/impt/pca_scores/Mobility_PCA_bike_Scores.csv")
impt_write(mob_walk_scores, "/impt/pca_scores/Mobility_PCA_walk_Scores.csv")

impt_write(pca_variance_report, "/impt/pca_scores/PCA_Variance_and_Correlations.csv")
impt_write(pca_summary_table, "/impt/pca_scores/PCA_Variance_Table.csv")

impt_write(
  data.frame(Indicator = names(Champions_access), Contribution = Champions_access),
  "/impt/pca_scores/Champions_Accessibility.csv"
)
impt_write(
  data.frame(Indicator = names(Champions_mobility), Contribution = Champions_mobility),
  "/impt/pca_scores/Champions_Mobility.csv"
)
impt_write(
  data.frame(Indicator = names(Champions_safety), Contribution = Champions_safety),
  "/impt/pca_scores/Champions_Safety.csv"
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

impt_write(Final_Results_Master, "/impt/results/IMPT_PCA_and_Entropy_Scores_freguesia.csv")


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

impt_write(Final_Results_Master_car, "/impt/results/IMPT_PCA_and_Entropy_Scores_car_freguesia.csv")
impt_write(Final_Results_Master_pt, "/impt/results/IMPT_PCA_and_Entropy_Scores_pt_freguesia.csv")
impt_write(Final_Results_Master_bike, "/impt/results/IMPT_PCA_and_Entropy_Scores_bike_freguesia.csv")
impt_write(Final_Results_Master_walk, "/impt/results/IMPT_PCA_and_Entropy_Scores_walk_freguesia.csv")


# =============================================================================
# SECTION 6 – SPATIAL AGGREGATION
# =============================================================================

# Population lookup: used to compute population-weighted means at higher levels
census24_fregmun_pop <- impt_read("census24_fregmun.csv", root = "useful_data") |>
  select(freg_id, mun_id, population) |>
  mutate(dtmnfr = as.integer(freg_id))

# Grid lookup: assigns each grid cell to a freguesia (generalised assumption)
grid_freg_mun <- impt_read("grid_nuts.csv", root = "useful_data") |>
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

impt_write(Final_Results_Master_municipio, "/impt/results/IMPT_PCA_and_Entropy_Scores_municipio.csv")
impt_write(Final_Results_Master_car_municipio, "/impt/results/IMPT_PCA_and_Entropy_Scores_car_municipio.csv")
impt_write(Final_Results_Master_pt_municipio, "/impt/results/IMPT_PCA_and_Entropy_Scores_pt_municipio.csv")
impt_write(Final_Results_Master_bike_municipio, "/impt/results/IMPT_PCA_and_Entropy_Scores_bike_municipio.csv")
impt_write(Final_Results_Master_walk_municipio, "/impt/results/IMPT_PCA_and_Entropy_Scores_walk_municipio.csv")


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

impt_write(Final_Results_Master_grid, "/impt/results/IMPT_PCA_and_Entropy_Scores_grid.csv")
impt_write(Final_Results_Master_grid_car, "/impt/results/IMPT_PCA_and_Entropy_Scores_car_grid.csv")
impt_write(Final_Results_Master_grid_pt, "/impt/results/IMPT_PCA_and_Entropy_Scores_pt_grid.csv")
impt_write(Final_Results_Master_grid_bike, "/impt/results/IMPT_PCA_and_Entropy_Scores_bike_grid.csv")
impt_write(Final_Results_Master_grid_walk, "/impt/results/IMPT_PCA_and_Entropy_Scores_walk_grid.csv")


# =============================================================================
# SECTION 7 – VISUALISATION  (requires 00_data_load.R to have loaded `freguesias`)
# =============================================================================

# source("code/00_data_load.R")   # uncomment if `freguesias` is not already in memory
freguesias <- impt_read("/geo/freguesias_2024_unique.gpkg")

impt_pca <- impt_read("/impt/results/IMPT_PCA_and_Entropy_Scores_freguesia.csv")

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
