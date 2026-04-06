# IMPT Data Processing Pipeline Flow

This directory contains the R scripts used for processing data, generating indicators, and aggregating the final scores for the Integrated Mobility Performance Tool (IMPT). 

The scripts are naturally organized in a sequential data processing pipeline. To clarify their precedents and dependencies, it is heavily suggested to prefix their filenames with a number. Below is the general flow and what each script achieves.

## General Flow & Suggested Numbering

### Phase 0: Initialization and Pre-processing
These scripts set up the environment and prepare base spatial or demographic datasets.

* **`00_data_load.R`**: Helper script that sets up global settings like data paths and defines functions to load and download datasets remotely. It handles the initial referencing for geometries and statistical data.
* **`01_data_prep.R`**: Core geometry preparation script. It handles administrative boundaries, road network downloading and filtering, processes OSM point of interest (POI) layers, parses GTFS transit routes, sets up the origins and destinations (via jittering), and prepares the building footprints.

### Phase 2: Socio-Demographics and Land Use
These scripts rely on base geometries to extract and calculate land use and census indicators. They can be executed in any order relative to each other.

* **`02_census_modalShare.R`**: Processes and cleans basic modal share data from Census 2021 responses.
* **`02_jobs_imob.R`**: Calculates job and employment data mobility patterns, relying on the IMOB (inquérito à mobilidade).
* **`02_land_use_export.R`**: Spatially aggregates housing, income, population, density, and demographic statistics to the grid, freguesia, and municipality levels.
* **`02_pois_at_scales.R`**: Computes point of interest (POI) densities and availabilities scaled at different geographic units.
* **`02_veh_ownership.R`**: Extracts and aggregates vehicle ownership averages at different geographic scales.

### Phase 3: Routing and Time/Distance Matrices
* **`03_ttm_gridh3.R`**: Uses the `r5r` (Rapid Realistic Routing on Real-world networks) engine configured earlier to compute the travel travel time matrices (TTM) and basic distances for multiple transport modes (car, PT, walk, bike) across origins to destinations.

### Phase 4: Dimension Calculations
These scripts parse outputs from Phase 2 and 3 to compute the exact indicators for the 4 core IMPT dimensions (Accessibility, Mobility, Affordability, Safety). 

* **`04_access_opportunities.R`**: Calculates general physical accessibility metrics by analyzing the ease of reaching POIs and opportunities.
* **`04_accessibility_gap.R`**: Evaluates accessibility gaps between distinct spatial zones along commuting constraints.
* **`04_mobility.R`**: Calculates infrastructural mobility statistics like infrastructure ratios and the integration of generic shared mobility networks.
* **`04_mobility_commuting.R`**: Focuses processing travel time arrays, trip generations, and modal availability primarily targeting peak commuting logic.
* **`04_mobility_transit.R`**: Aggregates complex public transit statistics (route frequencies, headways, and stop coverages).
* **`04_mobility_costs_money_car.R`**: Computes out-of-pocket financial expenditures (fuel, maintenance, etc.) expected for private commuting networks.
* **`04_mobility_costs_money_pt_cost_structure.r`**: Specific helpers mapping local fare policies for the entire transit grid.
* **`04_mobility_costs_money_pt.R`**: Evaluates monetary affordability strictly relating to PT use (e.g. single-fare vs overall transit passes).
* **`04_mobility_costs_time.R`**: Values and calculates generalized time costs dedicated across variable modes.
* **`04_safety.R`**: Computes road safety vulnerabilities by intersecting traffic crashes spatially, and grouping them against area-based population scales.

### Phase 5: Result Aggregation and Normalization
* **`05_IMPTcalculator.R`**: The core scoring engine. It loads the normalized dimension tables, scales and inverts them to percentages [0-100], calculates variance via Principal Component Analysis (PCA) and calculates weights through entropy scoring. It merges the output arrays to build the final IMPT general and sub-modal scores.

### Phase 6: Output Pipelines & Dashboarding
These scripts compile the calculated results strictly for system interaction and visualization tasks.

* **`06_results.R`**: Utility script to load and verify all the generated variables/results files from Phase 4 explicitly into memory.
* **`06_results_export.R`**: Exports calculated statistics matrices into independent dataframes.
* **`06_dashboard_export.R`**: The final integration point. It takes raw table indices joining them heavily with the visual geometries (grid cells, polygons). Formats columns precisely for the dashboard environment and uses `piggyback` to deploy the unified `GeoJSON` blocks straight to the live environment tracking platform (`IMPT-web`).
