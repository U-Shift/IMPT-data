# Cost of commuting by PT
# Purpose     Determine the generalized cost of PT commuting, considering the ODs of IMOB 2017
# Scales      hex, parish, municipality
# Issue       https://github.com/U-Shift/IMPT-data/issues/10


# Compute detailed itineraries -------------------------------------------------

# Start by loading r5r and the following variables from 03_ttm_gridh3.R
# > r5r_network, root_folder,

# Also, from 04_mobility_commuting.R
# > od_freguesias_jittered50, od_freguesias_jittered_OR_geo, od_freguesias_jittered_DE_geo

# From 03_ttm_gridh3.R
# fare_single, fare_pass, fare_single_2

mode <- "TRANSIT"
folder_name <- sprintf("%s/mobility_itineraries", root_folder)
if (!dir.exists(folder_name)) {
  dir.create(folder_name, recursive = TRUE)
}

max_walk_time <- 20 # Differs from 03_ttm_gridh3.R, which has computed for 15 minutes
fare_structures <- list("single" = fare_single, "pass" = fare_pass)
departure_datetime_HP <- as.POSIXct("04-02-2026 08:00:00", format = "%d-%m-%Y %H:%M:%S")

for (max_trip_duration in c(120)) {
  # For debug, 11352 has two buses
  # 11412 has bus + train
  message(paste("Running detailed itinerary for mode:", mode, "max trip duration:", max_trip_duration))

  # Static parameters
  args <- list()
  args$r5r_network <- r5r_network
  args$origins <- od_freguesias_jittered_OR_geo
  args$destinations <- od_freguesias_jittered_DE_geo

  # Varying parameters
  args$mode <- c(mode)
  args$max_trip_duration <- max_trip_duration

  args$max_rides <- 10 # Unlimited transfers
  # args$mode_egress default to WALk
  args$max_walk_time <- max_walk_time
  args$departure_datetime <- departure_datetime_HP

  args$drop_geometry <- TRUE # For efficiency
  args$all_to_all <- FALSE # Run only between each origin and its corresponding destination (same index in the list)

  args$verbose <- FALSE # For debug

  for (fs in names(fare_structures)) {
    args$fare_structure <- fare_structures[[fs]]

    output_csv <- sprintf("%s/itinerary_%s_%dmin", folder_name, tolower(mode), max_trip_duration)
    output_csv <- sprintf("%s_%s_fare", output_csv, fs)

    if (!dir.exists(output_csv)) {
      dir.create(output_csv, recursive = TRUE)
    }

    args$output_dir <- output_csv
    message("Running detailed itinerary... Storing output to ", output_csv)
    do.call(detailed_itineraries, args) # https://ipeagit.github.io/r5r/reference/detailed_itineraries.html
    message("Done :) Next...")
  }
}

# aggregate CSVs into single rds --------------------------------------------------------------

# Filter by those that contain "_fare"
dirs <- list.dirs(folder_name, recursive = FALSE) |>
  keep(~ grepl("_fare", ., ignore.case = TRUE))
dirs

for (folder in dirs) {
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

# For each itinerary, compute costs -------------------------------------------------


fare_base_file <- "itinerary_transit_120min_"
fares <- list("pass_fare", "single_fare")

for (fare in fares) {
  message("Computing costs for fare structure: ", fare)
  mobility_itineraries_disagregated <- impt_read(sprintf("mobility_itineraries/%s%s.rds", fare_base_file, fare))
  nrow(mobility_itineraries_disagregated)

  mobility_itineraries_costs <- mobility_itineraries_disagregated |>
    group_by(from_id, to_id) |>
    summarise(
      # Select first
      total_fare = first(total_fare),
      total_duration = first(total_duration),
      total_distance = first(total_distance),
      segments_n = n(),
      wait_time = sum(wait)
    )
  nrow(mobility_itineraries_costs) # 18066
  nrow(od_freguesias_jittered50) # 19547

  mobility_itineraries_costs_sf <- od_freguesias_jittered50 |>
    select(id, trips, Origin_dicofre24, Destination_dicofre24) |>
    left_join(
      mobility_itineraries_costs |>
        # Select only revelant columns from itineraries
        select(from_id, to_id, total_duration, total_distance, total_fare),
      by = c("id" = "from_id")
    ) |>
    filter(!is.na(total_fare))
  nrow(mobility_itineraries_costs_sf) # 18066

  class(mobility_itineraries_costs_sf)
  # >mapview::mapview(mobility_itineraries_sf |> filter(Origin_dicofre24!=Destination_dicofre24) |> sample_n(1), zcol="total_fare")

  summary(mobility_itineraries_costs)

  DATA_LOCATION <- "data" # When running locally, get data from local folder
  impt_write(mobility_itineraries_costs, sprintf("mobility_money_costs/mobility_itineraries_costs_pt_%s.csv", fare)

  # Aggregate by parish and municipality  -------------------------------------------------
  mobility_itineraries_costs_grid <- jittering_grid |>
    left_join(mobility_itineraries_costs, by = c("id" = "from_id")) |>
    filter(!is.na(total_fare)) # 1429 NAs (r5r could not compute itinerary, possibly because 120 min threshold)
  summary(mobility_itineraries_costs_grid)
  nrow(mobility_itineraries_costs_grid) # 17568

  aggregated_commuting_money_for_geometry <- function(grid) {
    return(
      grid |>
        summarise(
          # 1. Weighted total cost
          total_money = round(weighted.mean(total_fare, trips, na.rm = TRUE), digits = 2),
          total_money_return = total_money,
          total_money_2ways = total_money * 2,
          # 2. Weighted duration and distance
          avg_tt = round(weighted.mean(total_duration, trips, na.rm = TRUE), digits = 2),
          avg_distance = round(weighted.mean(total_distance, trips, na.rm = TRUE), digits = 2),
          # 3. Total trips for the group
          trips = round(sum(trips), digits = 2)
        ) |>
        ungroup() |>
        mutate(
          # When total_money is 0, set variables to NA
          total_money = ifelse(total_money == 0, NA, total_money),
          avg_tt = ifelse(total_money == 0, NA, avg_tt),
          avg_distance = ifelse(total_money == 0, NA, avg_distance),
          trips = ifelse(total_money == 0, NA, trips)
        )
    )
  }

  grid_commuting_money <- aggregated_commuting_money_for_geometry(mobility_itineraries_costs_grid |> group_by(id_grid_origin))
  summary(grid_commuting_money)

  grid_commuting_money_sf <- grid |>
    select(id, geom) |>
    left_join(grid_commuting_money, by = c("id" = "id_grid_origin"))
  # mapview(grid_commuting_money_sf, zcol = "total_money")
  # mapview(grid_commuting_money_sf, zcol = "avg_tt")
  # mapview(grid_commuting_money_sf, zcol = "avg_distance")

  freguesia_commuting_money <- aggregated_commuting_money_for_geometry(mobility_itineraries_costs_grid |> group_by(Origin_dicofre24))
  summary(freguesia_commuting_money)

  freguesia_commuting_money_sf <- freguesias |>
    select(dtmnfr, geom) |>
    left_join(freguesia_commuting_money, by = c("dtmnfr" = "Origin_dicofre24"))
  # mapview(freguesia_commuting_money_sf, zcol = "total_money")
  # mapview(freguesia_commuting_money_sf, zcol = "avg_tt")
  # mapview(freguesia_commuting_money_sf, zcol = "avg_distance")

  municipio_commuting_money <- aggregated_commuting_money_for_geometry(
    mobility_itineraries_costs_grid |>
      left_join(freguesias |> select(dtmnfr, municipio), by = c("Origin_dicofre24" = "dtmnfr")) |>
      group_by(municipio)
  )
  summary(municipio_commuting_money)

  municipio_commuting_money_sf <- municipios |>
    select(municipio, geom) |>
    left_join(municipio_commuting_money, by = c("municipio" = "municipio"))
  # mapview(municipio_commuting_money_sf, zcol = "total_money")
  # mapview(municipio_commuting_money_sf, zcol = "avg_tt")
  # mapview(municipio_commuting_money_sf, zcol = "avg_distance")

  output_dir <- "mobility_money_costs"
  impt_write(grid_commuting_money_sf, sprintf("%s/grid_commuting_money_pt_%s.gpkg", output_dir, fare)
  impt_write(grid_commuting_money, sprintf("%s/grid_commuting_money_pt_%s.csv", output_dir, fare)
  impt_write(freguesia_commuting_money_sf, sprintf("%s/freguesia_commuting_money_pt_%s.gpkg", output_dir, fare)
  impt_write(freguesia_commuting_money, sprintf("%s/freguesia_commuting_money_pt_%s.csv", output_dir, fare)
  impt_write(municipio_commuting_money_sf, sprintf("%s/municipio_commuting_money_pt_%s.gpkg", output_dir, fare)
  impt_write(municipio_commuting_money, sprintf("%s/municipio_commuting_money_pt_%s.csv", output_dir, fare)
  DATA_LOCATION <- "https://impt.server.ushift.pt" # When running locally, get data from remote server
}
