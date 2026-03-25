options(java.parameters = '-Xmx96G') # RAM to 16GB
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

# load fare structures for PT
fare_stucture_pass_file = tempfile(fileext = ".zip")
download.file(IMPT_URL("geo/r5r/fares_pass.zip"), fare_stucture_pass_file, mode = "wb")
fare_pass <- r5r::read_fare_structure(fare_stucture_pass_file)

fare_stucture_single_file = tempfile(fileext = ".zip")
download.file(IMPT_URL("geo/r5r/fares_single.zip"), fare_stucture_single_file, mode = "wb")
fare_single <- r5r::read_fare_structure(fare_stucture_single_file)

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
folder_name = sprintf("%s/ttm/ttm_%s_fare", root_folder, tolower(grid_name))
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
    if (mode == "TRANSIT") {
      departures = c(departure_datetime_HP, departure_datetime_FHP, departure_datetime_night)
    }
    for (departure_datetime in departures) {
      departure_datetime = as.POSIXct(departure_datetime, origin_tz = "Europe/Lisbon")
      args$departure_datetime = departure_datetime
      args$verbose = FALSE
      if (mode == "BICYCLE") {
        args$max_lts = max_lts
      }
      # > Transit has multiple max rides
      max_rides = c(NA)
      fare_structures = c(NA)
      max_fares = c(NA)
      if (mode == "TRANSIT") {
        args$mode_egress = mode_egress
        args$max_walk_time = max_walk_time
        max_rides = c(max_rides_1, max_rides_2, max_rides_3, max_rides_4, max_rides_5)
        fare_structures = list("single" = fare_single, "pass" = fare_pass, "single_2" = fare_single_2)
        max_fares = c(2,5,10)
      }
      
      for (mr in max_rides) {
        for (fs in names(fare_structures)) {
          for (mf in max_fares) {
            if (!is.na(mf)) {
              args$max_fare = mf
            }
            if (!is.na(fs)) {
              args$fare_structure = fare_structures[[fs]]
            }
            if (!is.na(mr)) {
              args$max_rides = mr
            }
            
            output_csv = sprintf("%s/ttm_%s_%dmin_%s", folder_name, tolower(mode), max_trip_duration, strftime(departure_datetime, "%Y%m%d%H%M", tz = "Europe/Lisbon"))
            if (!is.na(mr)) {
              output_csv = sprintf("%s_%dtransfers", output_csv, mr-1)
            }
            if (!is.na(fs)) {
              output_csv = sprintf("%s_%s_fare", output_csv, fs)
            }
            if (!is.na(mf)) {
              output_csv = sprintf("%s_maxfare%d", output_csv, mf)
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
  }
}

# aggregate CSVs into single rds --------------------------------------------------------------

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

# translate ttm into cost matrixes for PT --------------------------------------------------------------

## For transit, for each max_trip_duration, departure_datetime and fare type,
## left join progressively higher number of transfers to obtain travel cost matrix

folder_name
folder_name_costs = sprintf("%s/mobility_fare_costs", root_folder)
if(!dir.exists(folder_name_costs)) {
  dir.create(folder_name_costs, recursive = TRUE)
}

max_trip_durations = c(60,120)
departure_datetimes = c(departure_datetime_HP, departure_datetime_FHP, departure_datetime_night)
max_rides = c(max_rides_1, max_rides_2, max_rides_3, max_rides_4, max_rides_5)

fare_structures = list("single" = fare_single, "pass" = fare_pass, "single_2" = fare_single_2)
max_fares = c(2,5,10)


for (max_trip_duration in max_trip_durations) {
  for (departure_datetime in departure_datetimes) {
    for (mr in max_rides) {
      # Common attributes
      # ttm_transit_60min_202602040800_0transfers.rds
      
      # Fare variation
      for (fs in names(fare_structures)) {
        message(paste("Processing cost matrix for max trip duration:", max_trip_duration, "departure datetime:", departure_datetime, "max rides:", mr-1, "fare structure:", fs))
        ttm_with_cost = NA
        fare_structure = fare_structures[[fs]]
        for(mf in max_fares) {
          output_rds = sprintf("%s/ttm_%s_%dmin_%s_%dtransfers_%s_fare_maxfare%d.rds", 
                         folder_name, 
                         tolower(mode), max_trip_duration, strftime(departure_datetime, "%Y%m%d%H%M", tz = "Europe/Lisbon"),
                         mr-1, fs, mf
                       )
          ttm = readRDS(output_rds) |> mutate(cost=mf)
          if(any(is.na(ttm_with_cost))) {
            ttm_with_cost = ttm 
          } else {
            # bind ttm rows to ttm_with_cost, filtering ttm so that there is no matching (from_id, to_id)
            ttm_with_cost = bind_rows(
              ttm_with_cost,
              ttm |> anti_join(ttm_with_cost, by = c("from_id", "to_id"))
            )
          }
        }
        
        # Store travel cost matrix 
        cost_matrix_rds = sprintf("%s/cost_matrix_%s_%dmin_%s_%dtransfers_%s_fare.rds", 
          folder_name_costs, 
          tolower(mode), max_trip_duration, strftime(departure_datetime, "%Y%m%d%H%M", tz = "Europe/Lisbon"),
          mr-1, fs
        )
        saveRDS(ttm_with_cost, cost_matrix_rds)
      }
    }
  }
}


# stop r5r ----------------------------------------------------------------
r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)




