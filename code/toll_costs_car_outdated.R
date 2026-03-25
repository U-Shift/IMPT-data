# Compute toll costs for car trips between OD pairs (AML)
# Uses the osm_link_ids output from r5r's detailed_itineraries()
#
# Context:
#   - Routes were estimated with r5r (mode = "CAR") with osm_link_ids = TRUE
#   - r5r returns OSM *way* IDs (road segment edges) in osm_link_ids, NOT node IDs
#   - Toll prices are for 2026, Classe 1 (common car / ligeiro)
#   - Source: https://www.acp.pt/ResourcesUser/ACP/docs/Viagens_e_Lazer/Estrada_fora/Precos-Portagens-2026.pdf
#
# OSM toll way identification strategy:
#   - Open systems (pórtico/gantry): fixed fee each time a tagged way is traversed
#     e.g. way 22286596 = Ponte 25 de Abril deck (A2, toll=yes, oneway=yes S→N)
#          way 405372511 = Ponte Vasco da Gama deck (A12, toll=yes)
#   - Closed systems (praça/cabine): fee depends on entry + exit plaza (cumulative sections)
#   - Bridges: one-directional — only charged S→N. The oneway=yes tag on the OSM way
#     means r5r will only route through it in the payable direction, so no extra filtering needed.
#
# Lookup table source: useful_data/toll_plazas_aml_2026.csv
#   (edit that file to add/correct way IDs — change is immediately picked up here)
#
# Output:
#   - CSV with columns: from_id, to_id, toll_cost_eur
#   - Saved to: IMPT_URL("/mobility_fare_costs/toll_costs_car_od.csv")

library(dplyr)
library(purrr)
library(readr)
library(stringr)


# =============================================================================
# 1. LOAD TOLL PLAZA DEFINITIONS FROM CSV
# =============================================================================
# Source of truth: useful_data/toll_plazas_aml_2026.csv
# Columns: plaza_id, system, road, cost_eur_2026, notes, osm_way_ids (semicolon-separated)
#
# Key entries:
#   way 22286596  = Ponte 25 de Abril deck (A2, ref='A 2', toll=yes, oneway=yes S→N)
#   way 405372511 = Ponte Vasco da Gama deck (A12, ref='PVG;IP 1', toll=yes, oneway=yes S→N)
#
# Why way IDs, not node IDs?
#   r5r's detailed_itineraries(osm_link_ids = TRUE) returns OSM *way* IDs (edges),
#   not node IDs. The bridge/gantry ways are what appears in the route edge sequence.

toll_plazas_raw <- read_csv(
  IMPT_URL("/useful_data/toll_plazas_aml_2026.csv"),  # adjust if running locally
  col_types = cols(.default = "c")
)

# Expand semicolon-separated way IDs into one row per way
toll_plazas <- toll_plazas_raw %>%
  mutate(
    cost_eur_2026  = as.numeric(cost_eur_2026),
    osm_way_id_vec = str_split(osm_way_ids, ";")
  ) %>%
  unnest(osm_way_id_vec) %>%
  mutate(osm_way_id = str_trim(osm_way_id_vec)) %>%
  select(plaza_id, system, road, cost_eur_2026, notes, osm_way_id)


# ---- 1b. CLOSED SYSTEM TOLLS (fee depends on entry + exit node pair) --------
#
# The toll is charged at the EXIT booth, based on the entry point.
# We detect the FIRST and LAST toll nodes along the route that belong to the same concession.

# A1 (Autoestrada do Norte) — managed by Brisa
# Relevant AML sections: Alverca↔Carregado (section 1: 0.50€, section 2: 0.25€, etc.)
# Key toll plazas in AML:
#   - Alverca (A1/A9 junction): nodes 9706740785, 9706740786, 9706780314, 9706780321, seq.
#     (lat~38.888, lon~-9.055)
#   - V.Franca de Xira II: nodes 25391082, 25391083, 9879369305, 9879369309
#     (lat~38.887, lon~-9.054  — very close to Alverca, both at km boundary)
#
# A2 (Autoestrada do Sul) — managed by Brisa
# Relevant AML sections: Fogueteiro↔Coina (0.90€), Coina↔Palmela (0.85€)
# Key toll plazas in AML:
#   - Fogueteiro/Almada: nodes 6806187905, 6806187906, 1663258848, 1663259020
#     (lat~38.603-620, lon~-9.121-9.182) — at the south end, gantry approach to A2
#   - Coina: nodes 6806187907, 6806187908 (lat~38.620, lon~-9.182 area)
#     NOTE: exact location varies; use geographic cluster
#   - Palmela: nodes 196194889, 196194932, 9589545274, 9589545277
#     (lat~38.584, lon~-8.888)
#   - Pinhal Novo (A12): nodes 320145299, 25416674 (lat~38.662, lon~-8.894)
#   - Setúbal (A12): nodes 205038698, 205040715 (lat~38.568, lon~-8.866)
#
# A5 (Autoestrada da Costa do Estoril) — managed by Brisa
# CLOSED system with toll at EXIT in Cascais direction
# Entry plaza: Estádio Nacional junction (A5/A9)
# No OSM nodes found at the A5 entry plaza — it appears r5r routes via A5 will pass through
# the gantry nodes near Cascais end. Using geographic approach:
#   Nodes on A5: 3751374533, 3751374537 (lat~38.711, lon~-9.301 — near Malveira/A5-IC15)
#              : 3751410594, 3751417874 (lat~38.716, lon~-9.389 — near Cascais/IC15)
#              : 3950368056, 3950369214, etc. (lat~38.707-708, lon~-9.334 — mid A5)
#              : 258179637, 9588718278 (lat~38.713, lon~-9.292 — near Cruz Quebrada entry)
#              : 2327143697, 2327143698 (lat~38.818, lon~-9.331 — near A16 branch)
#              : 8968364919, 8968545818 (lat~38.819, lon~-9.187 — near Queluz/A9)
#
# A9 (CREL — Circular Regional Interior de Lisboa) — managed by Brisa
# Entirely a closed system with cumulative per-section fees
# Key plaza nodes near the main junction areas:
#   - Estádio Nacional (A5/A9) end: nodes 256516965, 256517355, 256517436, 26233726-28
#     (lat~38.742, lon~-9.275)
#   - Queluz: nodes 517256487, 522464085, 1935980350, 2812434001 (lat~38.771, lon~-9.275)
#   - A9/A16 (Radial Pontinha area): nodes 256507539, 256507739 (lat~38.866, lon~-9.138)
#   - Radial Pontinha: nodes 4879400075, 4879400082 (lat~38.767, lon~-9.146)
#   - A8/A9: nodes 2134394163, 2134394258 (lat~38.864, lon~-9.110)
#   - Bucelas/Zambujal: nodes 9648622694, 9648622698, 256514707, 256514835
#     (lat~38.788, lon~-9.240)
#   - A9/A10 junction: nodes 9102294799 (lat~38.656, lon~-9.221)
#   - Alverca: nodes 7662471386 (lat~38.674, lon~-9.230) — near A1/A9 junction
#
# A13 — through Benavente/Santo Estêvão — outside AML but accessible from it
#   - Santo Estêvão: nodes 957525479, 1106195967, 12958839328-30 (lat~38.840, lon~-8.727)

# Define cumulative costs from LISBON (city centre / northern Almada) for reference:
# These are used when building the closed-system matrix below.

# A9 (CREL) section-by-section costs (Classe 1, 2026):
# From ACP PDF:
#   1. Estádio Nacional (A5/A9) → Queluz:       0.35€
#   2. Queluz → A9/A16:                         0.30€
#   3. A9/A16 → Radial Pontinha:                0.35€
#   4. Radial Pontinha → Radial Odivelas:        0.75€
#   5. Radial Odivelas → A8/A9:                 0.40€
#   6. A8/A9 → Bucelas (Zambujal):              0.35€
#   7. Bucelas → A9/A10:                        0.90€
#   8. A9/A10 → Alverca:                        0.35€
# (Plus spur: A9/A10 → Arruda dos Vinhos: 0.75€, → Carregado: 1.20€, → Benavente: 1.55€)

# A1 sections in AML direction (Classe 1, 2026):
#   1. Alverca (A1/A9) → V.Franca de Xira II:   0.50€
#   2. V.Franca de Xira II → V.Franca de Xira I: 0.25€

# A2 sections in AML (Classe 1, 2026):
#   1. Fogueteiro → Coina:                       0.90€
#   2. Coina → Palmela:                          0.85€
#   3. Palmela → A2/A12:                         0.10€

# A5 sections (Classe 1, 2026):
#   1. Estádio Nacional → Oeiras:               0.40€
#   2. Oeiras → Carcavelos:                     0.70€
#   3. Carcavelos → Estoril:                    0.90€
#   4. Estoril → Cascais:                       0.50€

# A12 sections in AML (Classe 1, 2026):
#   1. Montijo → Pinhal Novo:                   1.10€
#   2. Pinhal Novo → Nó da A2/A12:              1.00€
#   3. Nó da A2/A12 → Setúbal:                  0.35€


# =============================================================================
# 2. COST MATRIX FOR CLOSED SYSTEMS
# =============================================================================
# Cumulative costs from the "entry reference" plaza for each closed-system road.
# Costs come from ACP PDF 2026, Classe 1.

# A9 (CREL) — order from Estádio Nacional (A5/A9) northward:
a9_order   <- c("a9_estadio","a9_queluz","a9_a16_pontinha","a9_radial_pontinha",
                "a9_a8_junction","a9_bucelas","a9_a10_junction","a9_alverca")
a9_cumcost <- c(0, 0.35, 0.65, 1.00, 1.40, 1.75, 2.65, 3.00)

# A1 — from Alverca northward:
a1_order   <- c("a1_alverca","a1_vfxira_ii")
a1_cumcost <- c(0, 0.50)

# A2 — from Fogueteiro southward:
a2_order   <- c("a2_fogueteiro","a2_coina","a2_palmela")
a2_cumcost <- c(0, 0.90, 1.75)

# A5 — from Cruz Quebrada (Lisbon end) westward to Cascais:
a5_order   <- c("a5_cruz_quebrada","a5_oeiras","a5_a16_junction","a5_estoril","a5_cascais")
a5_cumcost <- c(0, 0.40, 0.70, 1.10, 1.60)

# A12 — from Montijo toward Setúbal:
a12_order   <- c("a12_montijo","a12_pinhal_novo","a12_a2jct","a12_setubal")
a12_cumcost <- c(0, 1.10, 2.10, 2.45)


# =============================================================================
# 3. BUILD LOOKUP TABLES
# =============================================================================

build_closed_matrix <- function(order_vec, cumcost_vec) {
  expand.grid(entry = order_vec, exit_ = order_vec) %>%
    as_tibble() %>%
    mutate(
      entry = as.character(entry),
      exit_ = as.character(exit_),
      entry_idx = match(entry, order_vec),
      exit_idx  = match(exit_, order_vec),
      cost_eur_2026 = abs(cumcost_vec[exit_idx] - cumcost_vec[entry_idx])
    ) %>%
    filter(entry != exit_) %>%
    select(entry_plaza = entry, exit_plaza = exit_, cost_eur_2026)
}

closed_cost_matrix <- bind_rows(
  build_closed_matrix(a9_order, a9_cumcost),
  build_closed_matrix(a1_order, a1_cumcost),
  build_closed_matrix(a2_order, a2_cumcost),
  build_closed_matrix(a5_order, a5_cumcost),
  build_closed_matrix(a12_order, a12_cumcost)
)

# Separate open and closed way lookups
open_way_lookup <- toll_plazas %>%
  filter(system == "open") %>%
  select(plaza_id, osm_way_id)

closed_way_lookup <- toll_plazas %>%
  filter(system == "closed") %>%
  select(plaza_id, osm_way_id)

# Open plaza → cost table
open_plaza_costs <- toll_plazas %>%
  filter(system == "open") %>%
  distinct(plaza_id, cost_eur_2026)

# Plaza → road mapping (for closed system grouping)
plaza_to_road <- toll_plazas %>%
  distinct(plaza_id, road, system)


# =============================================================================
# 4. MAIN FUNCTION: compute toll cost for a single itinerary
# =============================================================================

#' Compute the toll cost for a set of OSM way IDs traversed in a trip
#'
#' @param osm_way_ids  Character vector of OSM way IDs from r5r detailed_itineraries
#'                     (the osm_link_ids column, split by comma)
#' @param open_lookup  Tibble mapping osm_way_id → plaza_id (open system)
#' @param closed_lookup Tibble mapping osm_way_id → plaza_id (closed system)
#' @param open_costs   Tibble: plaza_id → cost_eur_2026
#' @param closed_matrix Tibble: entry_plaza, exit_plaza → cost_eur_2026
#' @param plaza_road   Tibble: plaza_id → road
#' @return Numeric: total toll cost in EUR
compute_toll_cost <- function(osm_way_ids,
                               open_lookup, closed_lookup,
                               open_costs, closed_matrix, plaza_road) {
  total <- 0
  osm_way_ids <- as.character(osm_way_ids)

  # ---- OPEN SYSTEM ----
  # Charge once per open plaza whose tagged way appears in the route.
  # (Bridge ways like 22286596 are oneway=yes S→N, so r5r only uses them
  #  in the payable direction — no extra directional filter needed.)
  open_plazas_crossed <- open_lookup %>%
    filter(osm_way_id %in% osm_way_ids) %>%
    pull(plaza_id) %>%
    unique()

  open_cost <- open_costs %>%
    filter(plaza_id %in% open_plazas_crossed) %>%
    summarise(total = sum(cost_eur_2026, na.rm = TRUE)) %>%
    pull(total)

  total <- total + open_cost

  # ---- CLOSED SYSTEM ----
  # Find the first and last plaza on each road/concession, then look up the cost.
  closed_matched <- closed_lookup %>%
    filter(osm_way_id %in% osm_way_ids)

  if (nrow(closed_matched) >= 2) {
    # Position in route order (first occurrence of each way)
    way_position <- tibble(osm_way_id = osm_way_ids, position = seq_along(osm_way_ids))

    crossed_ordered <- closed_matched %>%
      left_join(way_position, by = "osm_way_id") %>%
      group_by(plaza_id) %>%
      slice_min(position, with_ties = FALSE) %>%
      ungroup() %>%
      left_join(plaza_road, by = "plaza_id") %>%
      arrange(position)

    closed_cost <- crossed_ordered %>%
      group_by(road) %>%
      summarise(
        entry_plaza = first(plaza_id),
        exit_plaza  = last(plaza_id),
        .groups     = "drop"
      ) %>%
      filter(entry_plaza != exit_plaza) %>%
      left_join(closed_matrix, by = c("entry_plaza", "exit_plaza")) %>%
      summarise(total = sum(cost_eur_2026, na.rm = TRUE)) %>%
      pull(total)

    total <- total + closed_cost
  }

  return(total)
}


# (Lookup tables already built in Section 3 above)


# =============================================================================
# 6. APPLY TO r5r ITINERARY DATA
# =============================================================================
# 
# Expected input: RDS file from detailed_itineraries() with osm_link_ids = TRUE
# Columns used: from_id, to_id, segment_id, osm_link_ids (comma-separated string)
#
# Adjust the path below to your actual itinerary RDS file.

itinerary_rds_path <- IMPT_URL("/mobility_fare_costs/itinerary_car_60min.rds")  # adjust as needed

message("Loading itinerary data...")
itineraries <- readRDS(itinerary_rds_path)

# The osm_link_ids column in r5r output is a comma-separated string of OSM way IDs.
# Parse and compute toll for each OD pair.

message("Computing toll costs per OD pair...")
od_toll_costs <- itineraries %>%
  # Parse the osm_link_ids string into a vector for each row
  mutate(
    osm_way_ids_vec = map(osm_link_ids, ~ {
      if (is.na(.x) || .x == "") return(character(0))
      str_split(.x, ",")[[1]] %>% str_trim()
    })
  ) %>%
  # Compute toll cost for each row (one leg/segment of the itinerary)
  mutate(
    toll_cost_eur = map_dbl(osm_way_ids_vec, ~ compute_toll_cost(
      osm_way_ids   = .x,
      open_lookup   = open_way_lookup,
      closed_lookup = closed_way_lookup,
      open_costs    = open_plaza_costs,
      closed_matrix = closed_cost_matrix,
      plaza_road    = plaza_to_road
    ))
  ) %>%
  # Aggregate to OD pair level (sum all segments/legs of the trip)
  group_by(from_id, to_id) %>%
  summarise(
    toll_cost_eur = sum(toll_cost_eur, na.rm = TRUE),
    .groups = "drop"
  )

message(sprintf("Computed toll costs for %d OD pairs", nrow(od_toll_costs)))
message(sprintf("  OD pairs with >0 toll: %d (%.1f%%)",
                sum(od_toll_costs$toll_cost_eur > 0),
                100 * mean(od_toll_costs$toll_cost_eur > 0)))
message(sprintf("  Mean toll (all pairs): %.3f €", mean(od_toll_costs$toll_cost_eur)))
message(sprintf("  Mean toll (tolled only): %.3f €",
                mean(od_toll_costs$toll_cost_eur[od_toll_costs$toll_cost_eur > 0])))

# Save output
output_path <- IMPT_URL("/mobility_fare_costs/toll_costs_car_od.csv")
write_csv(od_toll_costs, output_path)
message("Saved to: ", output_path)

# Quick sanity check: spot-check a few known tolled routes
# (e.g., trips that should cross Ponte 25 de Abril or CREL)
message("\n--- Spot check: top 10 highest toll OD pairs ---")
od_toll_costs %>%
  slice_max(toll_cost_eur, n = 10) %>%
  print()
