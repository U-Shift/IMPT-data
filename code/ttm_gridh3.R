# run ttm to all cell combinations and all modes

library(tidyverse)
library(sf)
options(java.parameters = '-Xmx30G') # RAM to 8GB
library(r5r)
library(interp)

# load network
r5r_network = build_network(data_path = "/data/IMPT/geo/r5r/") # already existing network model

# set r5r parameters
points = points_h3
nrow(points) # 3686
departure_datetime_HP = as.POSIXct("04-02-2026 08:00:00", format = "%d-%m-%Y %H:%M:%S")
departure_datetime_FHP = as.POSIXct("08-02-2026 20:00:00", format = "%d-%m-%Y %H:%M:%S")
departure_datetime_night = as.POSIXct("04-02-2026 03:00:00", format = "%d-%m-%Y %H:%M:%S")
max_trip_duration = 120 # 2 hours
mode_egress = "WALK"
max_walk_time = 15 # 20?
max_lts = 3 # for bike



# Car ---------------------------------------------------------------------

ttm_car = travel_time_matrix(r5r_network,
                             origins = points,
                             destinations = points,
                             mode = "CAR",
                             departure_datetime = departure_datetime_HP,
                             verbose = FALSE)

nrow(ttm_car) # 2822