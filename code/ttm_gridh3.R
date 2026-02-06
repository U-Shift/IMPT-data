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
# grid_name = "h3_res9" # takes 13min to run within rstudio-server, for transit.
mode_egress = "WALK"
max_walk_time = 15 # 20?
max_lts = 3 # for bike

# parameters to vary
departure_datetime_HP = as.POSIXct("04-02-2026 08:00:00", format = "%d-%m-%Y %H:%M:%S") 
departure_datetime_FHP = as.POSIXct("08-02-2026 20:00:00", format = "%d-%m-%Y %H:%M:%S") 
departure_datetime_night = as.POSIXct("04-02-2026 03:00:00", format = "%d-%m-%Y %H:%M:%S") 
max_trip_duration_120 = 120 # 2 hours
max_trip_duration_60 = 60 # 1 hours
max_rides_2 = 2 # 1 transfers
max_rides_3 = 3 # 2 transfers
use_csv = TRUE # except for TRANSIT


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
# # for runs with high ram ## FALTA VARIAR TAMBEM COM TRIPDURATION, senão overwrite :(
# if(!dir.exists(paste0(folder_name, "/out_csv")) & use_csv == TRUE) {
#   dir.create(paste0(folder_name, "/out_csv"), recursive = TRUE)
# }
  
for (mode in c("WALK")) { # c("CAR", "BICYCLE", "WALK", "TRANSIT")
  for (max_trip_duration in c(60, 120)) {
    message(paste("Running travel time matrix for mode:", mode, "max trip duration:", max_trip_duration))
    
    # Static parameters
    args = list()
    args$r5r_network = r5r_network
    args$origins = points
    args$destinations = points
    
    # Varying parameters
    args$mode = mode
    args$max_trip_duration = max_trip_duration 
    
    # # Car export to output_dir
    # if (mode == "CAR"){
    #   args$output_dir = paste0(folder_name, "/out_csv")
    # }
    
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
      if (mode == "TRANSIT") {
        args$mode_egress = mode_egress
        args$max_walk_time = max_walk_time
        max_rides = c(max_rides_2, max_rides_3)
      }
      
      for (mr in max_rides) {
        if (!is.na(mr)) {
          args$max_rides = mr
        }
        
        filename = sprintf("%s/ttm_%s_%dmin_%s", folder_name, tolower(mode), max_trip_duration, strftime(departure_datetime, "%Y%m%d%H%M", tz = "Europe/Lisbon"))
        if (!is.na(mr)) {
          filename = sprintf("%s_%dtransfers.rds", filename, mr-1)
        } else {
          filename = sprintf("%s.rds", filename)
        }
        ttm = do.call(travel_time_matrix, args)
        message("Saving to ", filename)
        saveRDS(ttm, filename)
        message("Done :)")
      }
    }
  }
}
# Now run also for the other modes! Se details for CAR


# load files --------------------------------------------------------------
# # For Car
# path_csv = paste0(folder_name, "/out_csv")
# # full.names = TRUE ensures the file paths include the directory name
# file_paths <- list.files(path = path_csv, pattern = "\\.csv$", full.names = TRUE)
# 
# # 2. Read all files and combine them into one data frame
# ttm_ <- map_dfr(file_paths, read_csv, show_col_types = FALSE) # This take some time (3min?)
# saveRDS(ttm_, paste0(folder_name, "/ttm_car_60min", ".rds"))
# rm(ttm_)
# # delete path_csv directory...
# fs::dir_delete(path_csv)
# 
# # the output files should be 12 +2 +1 +1 (PT, CAR, WALK, BIKE)

# load all ttm files
# Get a vector of all .rds file paths
file_paths <- list.files(
  path = folder_name,
  pattern = "\\.rds$",    # Use a regular expression to match files ending in .rds
  full.names = TRUE       # Returns the full path, essential for reading the files
)

# Load all files into a list
data_list <- lapply(file_paths, readRDS)



# stop r5r ----------------------------------------------------------------
r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)
