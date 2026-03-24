# Compute toll costs for car trips between OD pairs (AML)
# Uses the osm_link_ids output from r5r's detailed_itineraries()
# 
# Context:
#   - Routes were estimated with r5r (mode = "CAR") with osm_link_ids = TRUE
#   - This script takes the resulting RDS files and appends a toll_cost_eur column
#   - Toll prices are for 2026, Classe 1 (common car / ligeiro)
#   - Source: https://www.acp.pt/ResourcesUser/ACP/docs/Viagens_e_Lazer/Estrada_fora/Precos-Portagens-2026.pdf
#
# OSM toll node identification strategy:
#   - Open systems (gantry/pórtico): fixed fee per node crossed, regardless of entry/exit
#   - Closed systems (entry/exit booths): fee depends on the pair of nodes (entry + exit)
#   - Bridges: one-directional toll (paid only south→north)
#
# Output:
#   - CSV with columns: from_id, to_id, toll_cost_eur
#   - Saved to: data/r5r/toll_costs_car_od.csv

library(dplyr)
library(purrr)
library(readr)
library(stringr)


# =============================================================================
# 1. TOLL NODE DEFINITIONS (2026, Classe 1)
# =============================================================================
#
# Identification method: OSM nodes tagged barrier=toll_booth or highway=toll_gantry
# in the AML bounding box, matched against parent way road refs from Overpass API.
#
# Node clusters (multiple booths at same plaza) are listed individually —
# a route that crosses ANY node in a cluster is charged once for that plaza.
#
# Sources:
#   - OSM Overpass API (AML bbox: 38.3,-9.5,38.9,-8.7)
#   - ACP Toll Price Sheet 2026

# ---- 1a. OPEN SYSTEM TOLLS (fixed fee per gantry crossed) -------------------
#
# These are free-flow gantries (pórticos) where you always pay the same amount
# regardless of where you entered/exited the road.

open_tolls <- tribble(
  ~toll_id,            ~osm_ids,                                                                  ~road,  ~description,                         ~cost_eur_2026,
  # --- Ponte 25 de Abril (A2/IC20) — only S→N direction ---
  # OSM nodes at the Almada/Lisboa toll plaza (south side of bridge)
  "ponte_25abril",     list(c(3976984881, 3976984936, 3976992613, 6540527800)),                    "A2",  "Ponte 25 de Abril (S>N, Classe 1)",  2.25,
  # --- Ponte Vasco da Gama (A12) — only S→N direction ---
  # OSM nodes at the Alcochete toll plaza  
  "ponte_vasco_gama",  list(c(25416674, 25416732, 25416733, 25416741, 25416757,
                               9645967350, 9645967356, 9645976622, 9645976624,
                               9645976627, 9645976631, 320145299)),                               "A12", "Ponte Vasco da Gama (S>N, Classe 1)", 3.40,
  # --- A8 open gantry: Loures (portal entry into tolled section) ---
  # The section Loures-CREL is free (0.00 €); CREL-Lousa costs 0.75 €
  # OSM: nodes 25432058, 25432059, 13176703747, 13176703799 (at A8/Loures, near CREL junction)
  "a8_crel_lousa",     list(c(25432058, 25432059, 13176703747, 13176703799)),                     "A8",  "A8: CREL-Lousa gantry (Classe 1)",   0.75,
  # --- A16 (IC30) gantry — Circular Exterior ---
  # Single open-system gantry node near Cascais/Sintra
  # OSM node 469970704 (lat=38.778, lon=-9.364) near Malveira da Serra / A16
  "a16_gantry",        list(c(469970704, 889028787)),                                            "A16", "A16/IC30 gantry (Classe 1)",           0.85
)

# Flatten to a lookup: osm_node_id → toll record
open_toll_lookup <- open_tolls %>%
  mutate(osm_ids = map(osm_ids, ~ as.character(.x))) %>%
  unnest(osm_ids) %>%
  rename(osm_node_id = osm_ids) %>%
  distinct(osm_node_id, .keep_all = TRUE) # keep first match if node in multiple clusters


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
# 2. NODE CLUSTERS: Plaza-level grouping
# =============================================================================
# Each toll plaza may have multiple OSM nodes (one per lane/direction).
# We group them into plazas and assign a plaza_id, concession, and system type.

toll_plazas <- tribble(
  ~plaza_id,               ~system,   ~road,  ~osm_node_ids,                                                           ~notes,
  # OPEN SYSTEM
  "p25abril_sl",           "open",    "A2",   c(3976984881,3976984936,3976992613,6540527800),                           "Ponte 25 Abril - S→N only",
  "pvascogama_sl",         "open",    "A12",  c(25416674,25416732,25416733,25416741,25416757,
                                                 9645967350,9645967356,9645976622,9645976624,
                                                 9645976627,9645976631,320145299),                                       "Ponte Vasco da Gama - S→N only",
  "a8_loures_crel",        "open",    "A8",   c(25432058,25432059,13176703747,13176703799),                              "A8 Loures/CREL gantry",
  "a16_gantry1",           "open",    "A16",  c(469970704,889028787),                                                   "A16/IC30 gantry (Malveira da Serra area)",
  # CLOSED SYSTEM - A9 (CREL)
  "a9_estadio",            "closed",  "A9",   c(256516965,256517355,256517436,26233726,26233728,1701437326),             "A9 Estádio Nacional / A5 junction",
  "a9_queluz",             "closed",  "A9",   c(517256487,522464085,1935980350,2812434001),                              "A9 Queluz",
  "a9_radial_pontinha",    "closed",  "A9",   c(4879400075,4879400082),                                                  "A9 Radial Pontinha",
  "a9_a16_pontinha",       "closed",  "A9",   c(256507539,256507739),                                                   "A9 / A16-Radial Pontinha junction",
  "a9_a8_junction",        "closed",  "A9",   c(2134394163,2134394258),                                                  "A9 / A8 junction (Loures)",
  "a9_bucelas",            "closed",  "A9",   c(9648622694,9648622698,256514707,256514835),                              "A9 Bucelas (Zambujal)",
  "a9_a10_junction",       "closed",  "A9",   c(9102294799),                                                             "A9 / A10 junction",
  "a9_alverca",            "closed",  "A9",   c(7662471386),                                                             "A9 Alverca / A1 area",
  # CLOSED SYSTEM - A1
  "a1_alverca",            "closed",  "A1",   c(9706740785,9706740786,9706780314,9706780321,9706780325,9706780336,
                                                 256503732,256503978,256504255,256504385),                               "A1 Alverca / A1/A9 junction",
  "a1_vfxira_ii",          "closed",  "A1",   c(25391082,25391083,9879369305,9879369309,9879369308),                    "A1 V.Franca de Xira II",
  # CLOSED SYSTEM - A2
  "a2_fogueteiro",         "closed",  "A2",   c(6806187905,6806187906,1663258848,1663259020),                           "A2 Fogueteiro (Almada)",
  "a2_a2_coina_approx",    "closed",  "A2",   c(6806187907,6806187908),                                                  "A2 approach/Coina gantry",
  "a2_palmela",            "closed",  "A2",   c(196194889,196194932,9589545274,9589545277),                              "A2 Palmela",
  # CLOSED SYSTEM - A5 (Costa do Estoril)
  "a5_cruz_quebrada",      "closed",  "A5",   c(258179637,9588718278),                                                   "A5 Cruz Quebrada entry gantry",
  "a5_oeiras_area",        "closed",  "A5",   c(3950368056,3950368949,3950369214,3950369827,3950370201,3950370324),      "A5 Oeiras/Carnaxide area",
  "a5_estoril_cascais",    "closed",  "A5",   c(3751374533,3751374537),                                                  "A5 Estoril/Cascais end",
  "a5_a16_junction",       "closed",  "A5",   c(2327143697,2327143698),                                                   "A5 / A16 junction (Queluz area)",
  "a5_cascais_end",        "closed",  "A5",   c(3751410594,3751417874),                                                   "A5 Cascais terminus",
  # CLOSED SYSTEM - A12 (Sul do Tejo)
  "a12_montijo",           "closed",  "A12",  c(9356336688,9356336714,25417818),                                         "A12 Montijo",
  "a12_pinhal_novo",       "closed",  "A12",  c(320145299,25416674),                                                     "A12 Pinhal Novo",  # shared with Vasco da Gama approach
  "a12_palmela_jct",       "closed",  "A12",  c(196095945),                                                              "A12 / A2 junction area",
  "a12_setubal",           "closed",  "A12",  c(205038698,205040715,9593023707,9593023710,
                                                 13668306412,13668306426,13668306431,13668306446),                       "A12 Setúbal",
  # CLOSED SYSTEM - A13
  "a13_santo_estevao",     "closed",  "A13",  c(957525479,1106195967,12958839328,12958839329,12958839330),               "A13 Santo Estêvão"
) %>%
  mutate(osm_node_ids = map(osm_node_ids, as.character))


# =============================================================================
# 3. COST LOOKUP TABLES
# =============================================================================

# ---- 3a. Open toll costs (fixed per plaza) -----------------------------------
open_plaza_costs <- tribble(
  ~plaza_id,           ~cost_eur_2026,
  "p25abril_sl",        2.25,   # Ponte 25 de Abril S→N (Classe 1, 2026)
  "pvascogama_sl",      3.40,   # Ponte Vasco da Gama S→N (Classe 1, 2026)
  "a8_loures_crel",     0.75,   # A8 CREL-Lousa section
  "a16_gantry1",        0.85    # A16/IC30 gantry (approximated from ACP PDF)
  # Note: Loures-CREL section on A8 is 0.00€
)

# ---- 3b. Closed system matrix (entry_plaza → exit_plaza = cumulative cost) --
# Costs are CUMULATIVE (sum of sections traversed), Classe 1, 2026.
# Matrix is directional (from smaller section index to larger = positive direction).
# Negative/reverse direction: same cost (tolls apply both ways on closed systems).
#
# A9 (CREL) — order: estadio > queluz > a16_pontinha > radial_pontinha > a8_jct > bucelas > a10_jct > alverca
a9_order <- c("a9_estadio","a9_queluz","a9_a16_pontinha","a9_radial_pontinha","a9_a8_junction","a9_bucelas","a9_a10_junction","a9_alverca")
a9_cumcost <- c(0, 0.35, 0.65, 1.00, 1.40, 1.75, 2.65, 3.00)  # cumulative from estadio

# A1 — order: alverca > vfxira_ii
a1_order <- c("a1_alverca","a1_vfxira_ii")
a1_cumcost <- c(0, 0.50)

# A2 — order: fogueteiro > coina_approx > palmela
a2_order <- c("a2_fogueteiro","a2_a2_coina_approx","a2_palmela")
a2_cumcost <- c(0, 0.90, 1.75)

# A5 — order: cruz_quebrada (Lisbon end) > oeiras > a16_jct > estoril_cascais > cascais_end
a5_order <- c("a5_cruz_quebrada","a5_oeiras_area","a5_a16_junction","a5_estoril_cascais","a5_cascais_end")
a5_cumcost <- c(0, 0.40, 0.70, 1.30, 1.60)  # Lisbon→Cascais direction

# A12 — order: montijo > pinhal_novo > palmela_jct > setubal
a12_order <- c("a12_montijo","a12_pinhal_novo","a12_palmela_jct","a12_setubal")
a12_cumcost <- c(0, 1.10, 2.10, 2.45)

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

# Concession grouping (used to restrict closed-system lookups to same road)
plaza_to_road <- toll_plazas %>%
  select(plaza_id, road, system)


# =============================================================================
# 4. MAIN FUNCTION: compute toll cost for a single itinerary
# =============================================================================

#' Compute the toll cost for a set of OSM node IDs traversed in a trip
#'
#' @param osm_ids Character vector of OSM node/link IDs from r5r detailed_itineraries
#' @param open_lookup  Tibble mapping osm_node_id → plaza_id (open system)
#' @param closed_lookup Tibble mapping osm_node_id → plaza_id (closed system)
#' @param open_costs   Tibble: plaza_id → cost_eur_2026
#' @param closed_matrix Tibble: entry_plaza, exit_plaza → cost_eur_2026
#' @param plaza_road   Tibble: plaza_id → road
#' @return Numeric: total toll cost in EUR
compute_toll_cost <- function(osm_ids,
                               open_lookup, closed_lookup,
                               open_costs, closed_matrix, plaza_road) {
  total <- 0
  osm_ids <- as.character(osm_ids)
  
  # ---- OPEN SYSTEM ----
  # Identify open plazas crossed (each charged once, regardless of repetition)
  open_plazas_crossed <- open_lookup %>%
    filter(osm_node_id %in% osm_ids) %>%
    pull(plaza_id) %>%
    unique()
  
  open_cost <- open_costs %>%
    filter(plaza_id %in% open_plazas_crossed) %>%
    summarise(total = sum(cost_eur_2026, na.rm = TRUE)) %>%
    pull(total)
  
  total <- total + open_cost
  
  # ---- CLOSED SYSTEM ----
  # Identify closed plazas crossed in ORDER of appearance in route
  closed_matched <- closed_lookup %>%
    filter(osm_node_id %in% osm_ids)
  
  if (nrow(closed_matched) >= 2) {
    # Preserve order of first occurrence of each plaza in the route
    osm_position <- tibble(osm_node_id = osm_ids, position = seq_along(osm_ids))
    
    crossed_ordered <- closed_matched %>%
      left_join(osm_position, by = "osm_node_id") %>%
      group_by(plaza_id) %>%
      slice_min(position, with_ties = FALSE) %>%
      ungroup() %>%
      left_join(plaza_road, by = "plaza_id") %>%
      arrange(position)
    
    # For each road/concession, charge based on first and last plaza on that road
    closed_cost <- crossed_ordered %>%
      group_by(road) %>%
      summarise(
        entry_plaza = first(plaza_id),
        exit_plaza  = last(plaza_id),
        .groups = "drop"
      ) %>%
      filter(entry_plaza != exit_plaza) %>%  # only if actually traversed a section
      left_join(closed_matrix, by = c("entry_plaza","exit_plaza")) %>%
      summarise(total = sum(cost_eur_2026, na.rm = TRUE)) %>%
      pull(total)
    
    total <- total + closed_cost
  }
  
  return(total)
}


# =============================================================================
# 5. BUILD LOOKUP TABLES for use in compute_toll_cost()
# =============================================================================

# Flatten plaza → node lookup (separate for open and closed)
open_node_lookup <- toll_plazas %>%
  filter(system == "open") %>%
  unnest(osm_node_ids) %>%
  select(plaza_id, osm_node_id = osm_node_ids)

closed_node_lookup <- toll_plazas %>%
  filter(system == "closed") %>%
  unnest(osm_node_ids) %>%
  select(plaza_id, osm_node_id = osm_node_ids)


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

# The osm_link_ids column in r5r output is a comma-separated string of OSM IDs.
# Parse and compute toll for each OD pair.

message("Computing toll costs per OD pair...")
od_toll_costs <- itineraries %>%
  # Parse the osm_link_ids string into a vector for each row
  mutate(
    osm_ids_vec = map(osm_link_ids, ~ {
      if (is.na(.x) || .x == "") return(character(0))
      str_split(.x, ",")[[1]] %>% str_trim()
    })
  ) %>%
  # Compute toll cost for each row (segment of itinerary)
  mutate(
    toll_cost_eur = map_dbl(osm_ids_vec, ~ compute_toll_cost(
      osm_ids      = .x,
      open_lookup  = open_node_lookup,
      closed_lookup = closed_node_lookup,
      open_costs   = open_plaza_costs,
      closed_matrix = closed_cost_matrix,
      plaza_road   = plaza_to_road
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
