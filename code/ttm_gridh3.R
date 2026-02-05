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
nrow(points) # 3686 - this is the res 8 h3 grid
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



# Car ---------------------------------------------------------------------

# 1 hour
ttm_car_1h = travel_time_matrix(r5r_network,
                             origins = points,
                             destinations = points,
                             mode = "CAR",
                             departure_datetime = departure_datetime_HP,
                             max_trip_duration = max_trip_duration_60,
                             verbose = FALSE)

nrow(ttm_car_1h)
saveRDS(ttm_car_1h, "/data/IMPT/ttm/ttm_h3_res8/ttm_car_1h.rds")


# 2 hour
ttm_car_2h = travel_time_matrix(r5r_network,
                                origins = points,
                                destinations = points,
                                mode = "CAR",
                                departure_datetime = departure_datetime_HP,
                                max_trip_duration = max_trip_duration_120,
                                verbose = FALSE)

nrow(ttm_car_2h)
saveRDS(ttm_car_2h, "/data/IMPT/ttm/ttm_h3_res8/ttm_car_2h.rds")


# Bike --------------------------------------------------------------------

# 1 hour
ttm_bike_1h = travel_time_matrix(r5r_network,
                             origins = points,
                             destinations = points,
                             mode = "BICYCLE",
                             departure_datetime = departure_datetime_HP,
                             max_trip_duration = max_trip_duration_60,
                             max_lts = max_lts,
                             verbose = FALSE)

nrow(ttm_bike_1h)
saveRDS(ttm_bike_1h, "/data/IMPT/ttm/ttm_h3_res8/ttm_bike_1h.rds")

# 2 hour
ttm_bike_2h = travel_time_matrix(r5r_network,
                                 origins = points,
                                 destinations = points,
                                 mode = "BICYCLE",
                                 departure_datetime = departure_datetime_HP,
                                 max_trip_duration = max_trip_duration_120,
                                 max_lts = max_lts,
                                 verbose = FALSE)

nrow(ttm_bike_2h)
saveRDS(ttm_bike_2h, "/data/IMPT/ttm/ttm_h3_res8/ttm_bike_2h.rds")



# Walk --------------------------------------------------------------------

# 1 hour
ttm_walk_1h = travel_time_matrix(r5r_network,
                              origins = points,
                              destinations = points,
                              mode = "WALK",
                              departure_datetime = departure_datetime_HP,
                              max_trip_duration = max_trip_duration_60,
                              verbose = FALSE)

nrow(ttm_walk_1h)
saveRDS(ttm_walk_1h, "/data/IMPT/ttm/ttm_h3_res8/ttm_walk_1h.rds")

# 2 hour
ttm_walk_2h = travel_time_matrix(r5r_network,
                                 origins = points,
                                 destinations = points,
                                 mode = "WALK",
                                 departure_datetime = departure_datetime_HP,
                                 max_trip_duration = max_trip_duration_120,
                                 verbose = FALSE)

nrow(ttm_walk_2h)
saveRDS(ttm_walk_2h, "/data/IMPT/ttm/ttm_h3_res8/ttm_walk_2h.rds")



# PTransit ----------------------------------------------------------------

## 1 hour
# 1 hour, 1 transfer, HP
ttm_pt_1h_1t_HP = travel_time_matrix(r5r_network,
                                 origins = points,
                                 destinations = points,
                                 mode = "TRANSIT",
                                 departure_datetime = departure_datetime_HP,
                                 max_trip_duration = max_trip_duration_60,
                                 max_rides = max_rides_2,
                                 egress_mode = mode_egress,
                                 max_walk_time = max_walk_time,
                                 verbose = FALSE)

nrow(ttm_pt_1h_1t_HP)
saveRDS(ttm_pt_1h_1t_HP, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_1h_1t_HP.rds")

# 1 hour, 2 transfers, HP
ttm_pt_1h_2t_HP = travel_time_matrix(r5r_network,
                                 origins = points,
                                 destinations = points,
                                 mode = "TRANSIT",
                                 departure_datetime = departure_datetime_HP,
                                 max_trip_duration = max_trip_duration_60,
                                 max_rides = max_rides_3,
                                 egress_mode = mode_egress,
                                 max_walk_time = max_walk_time,
                                 verbose = FALSE)

nrow(ttm_pt_1h_2t_HP)
saveRDS(ttm_pt_1h_2t_HP, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_1h_2t_HP.rds")


# 1 hour, 1 transfer, FHP
ttm_pt_1h_1t_FHP = travel_time_matrix(r5r_network,
                                 origins = points,
                                 destinations = points,
                                 mode = "TRANSIT",
                                 departure_datetime = departure_datetime_FHP,
                                 max_trip_duration = max_trip_duration_60,
                                 max_rides = max_rides_2,
                                 egress_mode = mode_egress,
                                 max_walk_time = max_walk_time,
                                 verbose = FALSE)

nrow(ttm_pt_1h_1t_FHP)
saveRDS(ttm_pt_1h_1t_FHP, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_1h_1t_FHP.rds")

# 1 hour, 2 transfers, FHP
ttm_pt_1h_2t_FHP = travel_time_matrix(r5r_network,
                                 origins = points,
                                 destinations = points,
                                 mode = "TRANSIT",
                                 departure_datetime = departure_datetime_FHP,
                                 max_trip_duration = max_trip_duration_60,
                                 max_rides = max_rides_3,
                                 egress_mode = mode_egress,
                                 max_walk_time = max_walk_time,
                                 verbose = FALSE)

nrow(ttm_pt_1h_2t_FHP)
saveRDS(ttm_pt_1h_2t_FHP, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_1h_2t_FHP.rds")

# 1 hour, 1 transfer, night
ttm_pt_1h_1t_night = travel_time_matrix(r5r_network,
                                          origins = points,
                                          destinations = points,
                                          mode = "TRANSIT",
                                          departure_datetime = departure_datetime_night,
                                          max_trip_duration = max_trip_duration_60,
                                          max_rides = max_rides_2,
                                          egress_mode = mode_egress,
                                          max_walk_time = max_walk_time,
                                          verbose = FALSE)

nrow(ttm_pt_1h_1t_night)
saveRDS(ttm_pt_1h_1t_night, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_1h_1t_night.rds")

# 1 hour, 2 transfers, night
ttm_pt_1h_2t_night = travel_time_matrix(r5r_network,
                                          origins = points,
                                          destinations = points,
                                          mode = "TRANSIT",
                                          departure_datetime = departure_datetime_night,
                                          max_trip_duration = max_trip_duration_60,
                                          max_rides = max_rides_3,
                                          egress_mode = mode_egress,
                                          max_walk_time = max_walk_time,
                                          verbose = FALSE)

nrow(ttm_pt_1h_2t_night)
saveRDS(ttm_pt_1h_2t_night, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_1h_2t_night.rds")

## 2 hour
# 2 hour, 1 transfer, HP
ttm_pt_2h_1t_HP = travel_time_matrix(r5r_network,
                                        origins = points,
                                        destinations = points,
                                        mode = "TRANSIT",
                                        departure_datetime = departure_datetime_HP,
                                        max_trip_duration = max_trip_duration_120,
                                        max_rides = max_rides_2,
                                        egress_mode = mode_egress,
                                        max_walk_time = max_walk_time,
                                        verbose = FALSE)

nrow(ttm_pt_2h_1t_HP)
saveRDS(ttm_pt_2h_1t_HP, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_2h_1t_HP.rds")

# 2 hour, 2 transfers, HP
ttm_pt_2h_2t_HP = travel_time_matrix(r5r_network,
                                        origins = points,
                                        destinations = points,
                                        mode = "TRANSIT",
                                        departure_datetime = departure_datetime_HP,
                                        max_trip_duration = max_trip_duration_120,
                                        max_rides = max_rides_3,
                                        egress_mode = mode_egress,
                                        max_walk_time = max_walk_time,
                                        verbose = FALSE)

nrow(ttm_pt_2h_2t_HP)
saveRDS(ttm_pt_2h_2t_HP, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_2h_2t_HP.rds")


# 2 hour, 1 transfer, FHP
ttm_pt_2h_1t_FHP = travel_time_matrix(r5r_network,
                                          origins = points,
                                          destinations = points,
                                          mode = "TRANSIT",
                                          departure_datetime = departure_datetime_FHP,
                                          max_trip_duration = max_trip_duration_120,
                                          max_rides = max_rides_2,
                                          egress_mode = mode_egress,
                                          max_walk_time = max_walk_time,
                                          verbose = FALSE)

nrow(ttm_pt_2h_1t_FHP)
saveRDS(ttm_pt_2h_1t_FHP, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_2h_1t_FHP.rds")

# 2 hour, 2 transfers, FHP
ttm_pt_2h_2t_FHP = travel_time_matrix(r5r_network,
                                          origins = points,
                                          destinations = points,
                                          mode = "TRANSIT",
                                          departure_datetime = departure_datetime_FHP,
                                          max_trip_duration = max_trip_duration_120,
                                          max_rides = max_rides_3,
                                          egress_mode = mode_egress,
                                          max_walk_time = max_walk_time,
                                          verbose = FALSE)

nrow(ttm_pt_2h_2t_FHP)
saveRDS(ttm_pt_2h_2t_FHP, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_2h_2t_FHP.rds")

# 2 hour, 1 transfer, night
ttm_pt_2h_1t_night = travel_time_matrix(r5r_network,
                                            origins = points,
                                            destinations = points,
                                            mode = "TRANSIT",
                                            departure_datetime = departure_datetime_night,
                                            max_trip_duration = max_trip_duration_120,
                                            max_rides = max_rides_2,
                                            egress_mode = mode_egress,
                                            max_walk_time = max_walk_time,
                                            verbose = FALSE)

nrow(ttm_pt_2h_1t_night)
saveRDS(ttm_pt_2h_1t_night, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_2h_1t_night.rds")

# 2 hour, 2 transfers, night
ttm_pt_2h_2t_night = travel_time_matrix(r5r_network,
                                            origins = points,
                                            destinations = points,
                                            mode = "TRANSIT",
                                            departure_datetime = departure_datetime_night,
                                            max_trip_duration = max_trip_duration_120,
                                            max_rides = max_rides_3,
                                            egress_mode = mode_egress,
                                            max_walk_time = max_walk_time,
                                            verbose = FALSE)

nrow(ttm_pt_2h_2t_night)
saveRDS(ttm_pt_2h_2t_night, "/data/IMPT/ttm/ttm_h3_res8/ttm_pt_2h_2t_night.rds")




# stop r5r ----------------------------------------------------------------

r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)
