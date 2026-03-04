options(java.parameters = '-Xmx64G') # RAM to 16GB
library(r5r)

# r5r
r5r_location = IMPT_URL("/geo/r5r/")
# Download files for network previously built to temp dir, for local use
r5r_temp_dir = tempdir()
download_remote_file(r5r_location, "network.dat", r5r_temp_dir)
download_remote_file(r5r_location, "network_settings.json", r5r_temp_dir)
download_remote_file(r5r_location, "GLPS_DEM_COPERNICUS_30_DEM_2026.tif", r5r_temp_dir)
# List files in r5r_temp_dir
list.files(r5r_temp_dir)

# r5r_network = r5r::build_network(r5r_location, verbose = FALSE)
r5r_network = r5r::build_network(r5r_temp_dir, verbose = FALSE)

# Attention! Stop here. Run the code below only when you have finished using r5r, to free up memory :)
r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)

# run ttm to all cell combinations and all modes

library(tidyverse)

# load network, variable r5r_network using data_load.R
r5r_network

# set r5r parameters
# root_folder = "/data/IMPT"
root_folder = "data" # Set to "/data/IMPT/ when running at server.ushift.pt, or "data" when running locally
points = points_h3
nrow(points) # 3686 - this is the res 8 h3 grid, and 25890 for res 9
grid_name = "h3_res8" # fast to run, but 13.5Million combinations!. takes 17min for each run
mode_egress = "WALK"
max_walk_time = 15 # 20?
max_lts = 3 # for bike

# parameters to vary
departure_datetime_HP = as.POSIXct("04-02-2026 08:00:00", format = "%d-%m-%Y %H:%M:%S") 
departure_datetime_FHP = as.POSIXct("08-02-2026 20:00:00", format = "%d-%m-%Y %H:%M:%S") 
departure_datetime_night = as.POSIXct("04-02-2026 03:00:00", format = "%d-%m-%Y %H:%M:%S") 
max_trip_duration_240 = 240 # 4 hours
max_trip_duration_120 = 120 # 2 hours
max_trip_duration_60 = 60 # 1 hours
max_rides_1 = 1 # direct only, no transfers
max_rides_2 = 2 # 1 transfers
max_rides_3 = 3 # 2 transfers
max_rides_4 = 4 # 3 transfers
max_rides_5 = 5 # 4 transfers

# # manual run for CAR
# # ttm_car_60min = 
#   travel_time_matrix(r5r_network,
#                                 origins = points,
#                                 destinations = points,
#                                 mode = "CAR",
#                                 departure_datetime = departure_datetime_HP,
#                                 max_trip_duration = max_trip_duration_60,
#                                 output_dir = "data/ttm/ttm_h3_res8/out_csv",
#                                 verbose = FALSE,
#                                 progress = TRUE)


# run for different modes -------------------------------------------------

# main()
folder_name = sprintf("%s/ttm/ttm_%s", root_folder, tolower(grid_name))
if(!dir.exists(folder_name)) {
  dir.create(folder_name, recursive = TRUE)
}
  
for (mode in c("TRANSIT")) { # "CAR", "BICYCLE", "WALK", 
  for (max_trip_duration in c(60,120)) {
    message(paste("Running travel time matrix for mode:", mode, "max trip duration:", max_trip_duration))
    
    # Static parameters
    args = list()
    args$r5r_network = r5r_network
    args$origins = points
    args$destinations = points
    
    # Varying parameters
    args$mode = mode
    args$max_trip_duration = max_trip_duration 
    
    # > Transit has multiple departure times
    departures = c(departure_datetime_HP)
    # if (mode == "TRANSIT") {
    #   departures = c(departure_datetime_HP, departure_datetime_FHP, departure_datetime_night)
    # }
    for (departure_datetime in departures) {
      departure_datetime = as.POSIXct(departure_datetime, origin_tz = "Europe/Lisbon")
      args$departure_datetime = departure_datetime
      args$verbose = FALSE
      if (mode == "BICYCLE") {
        args$max_lts = max_lts
      }
      # > Transit has multiple max rides
      max_rides = c(NA)
      if (mode == "TRANSIT") {
        args$mode_egress = mode_egress
        args$max_walk_time = max_walk_time
        max_rides = c(max_rides_1)
      }
      
      for (mr in max_rides) {
        if (!is.na(mr)) {
          args$max_rides = mr
        }
        
        output_csv = sprintf("%s/ttm_%s_%dmin_%s", folder_name, tolower(mode), max_trip_duration, strftime(departure_datetime, "%Y%m%d%H%M", tz = "Europe/Lisbon"))
        if (!is.na(mr)) {
          output_csv = sprintf("%s_%dtransfers", output_csv, mr-1)
        }
        if(!dir.exists(output_csv)) {
          dir.create(output_csv, recursive = TRUE)
        }
        
        args$output_dir = output_csv
        message("Running ttm... Storing output to ", output_csv)
        do.call(travel_time_matrix, args)
        message("Done :) Next mode...")
      }
    }
  }
}

# aggregate csvs into single rds --------------------------------------------------------------

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

# stop r5r ----------------------------------------------------------------
r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)

