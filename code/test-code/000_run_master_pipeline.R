# Master Pipeline Script for IMPT Data Processing
# This script runs all the individual R scripts in the correct order.
# Some scripts, particularly those related to routing (r5r) and matrix calculations,
# will take a significantly longer time to run. Messages are printed before these.

# Function to run a script and time it
run_script <- function(script_name, description = "", is_long = FALSE) {
  message(sprintf("======================================================"))
  message(sprintf("=> STARTING: %s", script_name))
  if (description != "") message(sprintf("=> Task: %s", description))

  if (is_long) {
    message("=> \u26A0\uFE0F WARNING: This script is computationally intensive and may take a long time (minutes to hours depending on the grid/network size).")
  }

  start_time <- Sys.time()

  # Run the script
  # We use local = TRUE to avoid polluting the global environment if possible,
  # or local = FALSE if scripts depend on variables from previous scripts.
  # Assuming scripts are meant to be run in the global environment:
  tryCatch(
    {
      source(script_name, local = FALSE)
      end_time <- Sys.time()
      time_taken <- round(difftime(end_time, start_time, units = "mins"), 2)
      message(sprintf("=> COMPLETED: %s in %s minutes", script_name, time_taken))
    },
    error = function(e) {
      message(sprintf("\n[ERROR] Script %s failed to run! Aborting pipeline.", script_name))
      message(e)
      stop("Pipeline aborted.")
    }
  )

  message(sprintf("======================================================\n"))
}

# Ensure working directory is set to where the script is located
# setwd("/media/rosa/Dados/GIS/IMPT-data/code")

message("Starting IMPT Master Pipeline...\n")

# --- PHASE 0: Data Handling and Loading ---
# CHOOSE ONLY ONE OF THE FOLLOWING TWO SCRIPTS:
# Use 'internal' if you are on the project team (or have the API key/server access).
# Use 'external' if you are an external user.
# run_script("00a_impt_data_handle_external.R", "Handling external data sources") # choose one of these two
run_script("00a_impt_data_handle_internal.R", "Handling internal data sources") # choose one of these two
run_script("00b_data_load.R", "Loading initial data")

# --- PHASE 1: Data Preparation ---
run_script("01_data_prep.R", "Preparing network, geometries, and OSM data")

# --- PHASE 2: Spatial & Census Data Aggregation ---
run_script("02_census_grid_with_cos.R", "Intersecting Census with grid and land use", is_long = TRUE)
run_script("02_census_modalShare.R", "Calculating modal shares")
run_script("02_jobs_imob.R", "Processing jobs and real estate data")
run_script("02_land_use_export.R", "Exporting land use layers")
run_script("02_pois_at_scales.R", "Aggregating Points of Interest")
run_script("02_veh_ownership.R", "Calculating vehicle ownership metrics")

# --- PHASE 3: Routing and Travel Time Matrices (r5r) ---
run_script("03_ttm_gridh3.R", "Calculating Travel Time Matrices (TTM) via r5r", is_long = TRUE)

# --- PHASE 4: Mobility, Accessibility & Costs ---
# Note: Many of these depend on the TTMs computed in Phase 3
run_script("04_isochrones_PTstops.R", "Generating PT stop isochrones (r5r)", is_long = TRUE)
run_script("04_access_opportunities.R", "Calculating accessibility to opportunities (Jobs, POIs)", is_long = TRUE)
run_script("04_accessibility_gap.R", "Calculating accessibility gaps between modes")
run_script("04_affordability.R", "Calculating affordability metrics")
run_script("04_mobility.R", "Processing general mobility indicators")
run_script("04_mobility_bike_infrastructure.R", "Processing cycling infrastructure metrics")
run_script("04_mobility_commuting.R", "Commuting scenarios & transfers analysis via TTMs", is_long = TRUE)
run_script("04_mobility_costs_money_car.R", "Car monetary cost modeling")
run_script("04_mobility_costs_money_pt_cost_structure.r", "PT fare structure analysis")
run_script("04_mobility_costs_money_pt.R", "PT monetary cost modeling")
run_script("04_mobility_costs_time.R", "Travel time cost modeling across scales", is_long = TRUE)
run_script("04_mobility_transit.R", "Transit frequency and service levels")
run_script("04_mobility_transit_population.R", "Transit stops capacity and population served")
run_script("04_safety.R", "Road safety and crash analysis")

# --- PHASE 5: IMPT Index Calculation ---
run_script("05_IMPTcalculator.R", "Calculating final PCA-based IMPT dimensions and composite index", is_long = FALSE)

# --- PHASE 6: Results Export ---
run_script("06a_results_load.R", "Loading computed IMPT results")
run_script("06b_dashboard_export.R", "Exporting data to GeoJSON for the web dashboard")

# Optional:
# run_script("07_workshop-survey.R", "Processing workshop survey data")

message("IMPT Master Pipeline completed successfully!")
