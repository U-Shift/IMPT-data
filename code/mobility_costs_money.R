# Compute detailed itineraries -------------------------------------------------

# Start by loading r5r and the following variables from ttm_gridh3.R
# > r5r_network, root_folder, points

mode = "CAR"
folder_name = sprintf("%s/mobility_fare_costs", root_folder)

for (max_trip_duration in c(60,120)) {
  message(paste("Running detailed itinerary for mode:", mode, "max trip duration:", max_trip_duration))
  
  # Static parameters
  args = list()
  args$r5r_network = r5r_network
  args$origins = points
  args$destinations = points
  
  # Varying parameters
  args$mode = mode
  args$max_trip_duration = max_trip_duration 

  args$verbose = FALSE
  args$drop_geometry = TRUE # To make it more efficient
  args$osm_link_ids = TRUE
        
  output_csv = sprintf("%s/itinerary_%s_%dmin", folder_name, tolower(mode), max_trip_duration)
  if(!dir.exists(output_csv)) {
    dir.create(output_csv, recursive = TRUE)
  }
  
  args$output_dir = output_csv
  message("Running detailed itinerary... Storing output to ", output_csv)
  do.call(detailed_itineraries, args) # https://ipeagit.github.io/r5r/reference/detailed_itineraries.html
  message("Done :) Next...")
}

# Aggregate CSVs into single rds --------------------------------------------------------------

for (folder in list.dirs(folder_name, recursive = FALSE)) {
  message("Aggregating CSVs in folder: ", folder)
  
  # Get all CSV file paths in the folder
  csv_files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
  
  # Read and combine all CSV files into one data frame
  ttm_combined <- map_dfr(csv_files, read_csv, show_col_types = FALSE)
  
  # Save the combined data frame as an RDS file
  rds_file_path <- paste0(folder, ".rds")
  saveRDS(ttm_combined, rds_file_path)
  
  # Clean up: remove the original CSV files
  # file.remove(csv_files)
  
  message("Finished aggregating and cleaning up for folder: ", folder)
}

# For each rds, assign costs -------------------------------------------------

cost_per_km = 0.4 # https://diariodarepublica.pt/dr/detalhe/portaria/1553-d-2008-243733

tools_top_up = list(
  # osm_id: cost (€)
  "22286596" = 2.25, # Ponte 25 de Abril, sentido Sul>Norte
  "405372511" = 3.40 # Ponte Vasco da Gama, sentido Sul>Norte
) # There are more tools... https://www.acp.pt/ResourcesUser/ACP/docs/Viagens_e_Lazer/Estrada_fora/Precos-Portagens-2026.pdf

# TODO From here, considering data structure returned by detailed_itineraries, assign costs to each itinerary based on distance and tolls (tool is detected considering the list of osm ids returned for each itinerary)


