# Cost of commuting by car
# Purpose     Determine the generalized cost of car commuting, considering the ODs of IMOB 2017
# Scales      hex, parish, municipality
# Issue       https://github.com/U-Shift/IMPT-data/issues/10


# Compute detailed itineraries -------------------------------------------------

# Start by loading r5r and the following variables from 03_ttm_gridh3.R
# > r5r_network, root_folder

# Also, from 04_mobility_commuting.R
# > od_freguesias_jittered50, od_freguesias_jittered_OR_geo, od_freguesias_jittered_DE_geo

mode <- "CAR"
folder_name <- sprintf("%s/mobility_itineraries", root_folder)
if (!dir.exists(folder_name)) {
  dir.create(folder_name, recursive = TRUE)
}

for (max_trip_duration in c(60)) {
  message(paste("Running detailed itinerary for mode:", mode, "max trip duration:", max_trip_duration))

  # Static parameters
  args <- list()
  args$r5r_network <- r5r_network
  args$origins <- od_freguesias_jittered_OR_geo
  args$destinations <- od_freguesias_jittered_DE_geo

  # Varying parameters
  args$mode <- c(mode)
  args$max_trip_duration <- max_trip_duration

  args$verbose <- FALSE
  args$drop_geometry <- FALSE # To set osm_link_ids to `TRUE`, the parameter 'drop_geometry' must also be `FALSE`
  args$osm_link_ids <- TRUE
  args$all_to_all <- FALSE # Run only between each origin and its corresponding destination (same index in the list)

  args$verbose <- FALSE # For debug

  output_csv <- sprintf("%s/itinerary_%s_%dmin", folder_name, tolower(mode), max_trip_duration)
  if (!dir.exists(output_csv)) {
    dir.create(output_csv, recursive = TRUE)
  }

  args$output_dir <- output_csv
  message("Running detailed itinerary... Storing output to ", output_csv)
  do.call(detailed_itineraries, args) # https://ipeagit.github.io/r5r/reference/detailed_itineraries.html
  message("Done :) Next...")
}

# Aggregate CSVs into single rds --------------------------------------------------------------
library(tidyverse)

# Define a function to handle the "smart" splitting
read_custom_csv <- function(file_path) {
  lines <- read_lines(file_path)

  # The fix: We use a more explicit set for the lookahead.
  # This looks for commas NOT followed by a closing paren or bracket
  # without an opening one appearing first.
  split_pattern <- ",(?![^\\(\\[]*[\\)\\]])"

  data <- str_split(lines, split_pattern) %>%
    map_dfr(~ as_tibble_row(set_names(.x, paste0("V", seq_along(.x)))))

  colnames(data) <- as.character(data[1, ])
  data <- data[-1, ] |>
    type_convert()

  return(data)
}

for (folder in list.dirs(folder_name, recursive = FALSE)) {
  message("Aggregating CSVs in folder: ", folder)

  # Get all CSV file paths in the folder
  csv_files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)

  # Read and combine all CSV files into one data frame
  ttm_combined <- map_dfr(csv_files, read_custom_csv)

  # Save the combined data frame as an RDS file
  rds_file_path <- paste0(folder, ".rds")
  saveRDS(ttm_combined, rds_file_path)

  # Clean up: remove the original CSV files
  # file.remove(csv_files)

  message("Finished aggregating and cleaning up for folder: ", folder)
}


# |>
#   mutate(
#     # Remove "[" and "]" and split by ",", casting to int
#     osm_id_list_int = str_remove_all(osm_id_list, "\\[|\\]") |>
#       str_split(",") |>
#       map(~ as.integer(.x))
#   )

# For each itinerary, compute costs -------------------------------------------------

mobility_itineraries <- readRDS_remote(IMPT_URL("mobility_itineraries/itinerary_car_120min.rds"))
cost_per_km <- 0.4 # https://diariodarepublica.pt/dr/detalhe/portaria/1553-d-2008-243733

mobility_itineraries_sf <- mobility_itineraries |>
  # Select only revelant columns from itineraries
  select(from_id, to_id, total_duration, total_distance, geometry) |>
  # Recover trips weight from jittering
  left_join(od_freguesias_jittered50 |> select(id, trips, Origin_dicofre24, Destination_dicofre24) |> st_drop_geometry(), by = c("from_id" = "id")) |>
  # Convert to sf, using column geometry, a column with syntax: LINESTRING (x1 y1, x2 y2, ...)
  st_as_sf(wkt = "geometry", crs = 4326)

class(mobility_itineraries_sf)
# mapview::mapview(mobility_itineraries_sf |> filter(Origin_dicofre24!=Destination_dicofre24) |> sample_n(1))

library(jsonlite)
compute_tools_for_itinerary <- function(sf_itinerary) {
  # sf_itinerary =  mobility_itineraries_sf |> filter(Origin_dicofre24!=Destination_dicofre24) |> sample_n(1) # For debug only

  # Create string with aray of arrays for geometry: [[x1, y1], [x2, y2], ...]]
  geometry_array <- sf_itinerary$geometry[[1]] |>
    st_coordinates() |>
    as.data.frame() |>
    select(X, Y) |>
    as.matrix() |>
    toJSON(auto_unBOX = FALSE)

  # Make POST request to https://portagens.infraestruturasdeportugal.pt/Portagens.asmx/ObterCustoPortagens, with the following attributes
  # JSON body: {"coordenadas": [[x1, y1], [x2, y2], ...]]}
  # Headers: Referer: https://portagens.infraestruturasdeportugal.pt/
  response <- httr::POST(
    url = "https://portagens.infraestruturasdeportugal.pt/Portagens.asmx/ObterCustoPortagens",
    body = list(coordenadas = geometry_array),
    encode = "json",
    httr::add_headers(Referer = "https://portagens.infraestruturasdeportugal.pt/")
  )
  # Example response: {"d":[2.25,4.85,6.55,8.45]}
  costs <- fromJSON(httr::content(response, "text"))

  # Convert to data.frame with columns: C1, C2, C3 and C4
  costs_df <- as.data.frame(t(costs$d))
  colnames(costs_df) <- c("C1", "C2", "C3", "C4")
  return(costs_df)
}

# For each itinerary, compute costs and add to the data frame
mobility_itineraries_tools <- mobility_itineraries_sf |>
  # filter(Origin_dicofre24!=Destination_dicofre24) |> sample_n(10) |> # For debug only
  rowwise() |>
  mutate(tools = compute_tools_for_itinerary(cur_data())) |>
  unnest_wider(tools, names_sep = "_") |>
  st_droiop_geometry()

nrow(mobility_itineraries_tools |> filter(tools_C1 != 0)) # 2188
mapview::mapview(mobility_itineraries_sf |> left_join(mobility_itineraries_tools, by = c("from_id", "to_id")) |> filter(tools_C1 != 0), zcol = "tools_C1")

# skimr::skim(mobility_itineraries_tools)
summary(mobility_itineraries_tools)
summary(mobility_itineraries_tools |> filter(tools_C1 != 0))
# mapview::mapview(mobility_itineraries_sf |> left_join(mobility_itineraries_tools, by=c("from_id", "to_id")) |> filter(tools_C1!=0), zcol="tools_C1")
# mapview::mapview(mobility_itineraries_sf |> left_join(mobility_itineraries_tools, by=c("from_id", "to_id")) |> filter(tools_C1>0 & tools_C1<2) |> sample_n(1), zcol="tools_C1")


mobility_itineraries_costs <- mobility_itineraries_tools |>
  rename(cost_tools = tools_C1) |>
  select(-starts_with("tools_")) |>
  mutate(
    cost_tools = round(cost_tools, digits = 2),
    cost_distance = round(total_distance / 1000 * cost_per_km, digits = 2),
    total_cost = cost_tools + cost_distance
  )
summary(mobility_itineraries_costs)

# mapview::mapview(mobility_itineraries_sf |> left_join(mobility_itineraries_costs, by=c("from_id", "to_id")) |> filter(total_cost>30) |> sample_n(1), zcol="tools_C1")

write.csv(mobility_itineraries_costs, IMPT_URL("mobility_money_costs/mobility_itineraries_costs.csv"), row.names = FALSE)


# Aggregate by parish and municipality  -------------------------------------------------
mobility_itineraries_costs <- read.csv(IMPT_URL("mobility_money_costs/mobility_itineraries_costs.csv"))
mobility_itineraries_costs_grid <- jittering_grid |>
  select(-Origin_dicofre24, -Destination_dicofre24, -trips) |>
  left_join(mobility_itineraries_costs, by = c("id" = "from_id")) |>
  filter(!is.na(total_cost)) # 115 NAs (r5r could not compute itinerary, possibly because 120 min threshold)
summary(mobility_itineraries_costs_grid)

aggregated_commuting_money_for_geometry <- function(grid) {
  return(
    grid |>
      summarise(
        # 1. Weighted total cost
        total_money = round(weighted.mean(total_cost, trips, na.rm = TRUE), digits = 2),
        # 2. Weighted duration and distance
        avg_tt = round(weighted.mean(total_duration, trips, na.rm = TRUE), digits = 2),
        avg_distance = round(weighted.mean(total_distance, trips, na.rm = TRUE), digits = 2),
        # 3. Total trips for the group
        trips = round(sum(trips), digits = 2)
      ) |>
      ungroup()
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
  mutate(dtmnfr = as.integer(dtmnfr)) |>
  left_join(freguesia_commuting_money, by = c("dtmnfr" = "Origin_dicofre24"))
# mapview(freguesia_commuting_money_sf, zcol = "total_money")
# mapview(freguesia_commuting_money_sf, zcol = "avg_tt")
# mapview(freguesia_commuting_money_sf, zcol = "avg_distance")

municipio_commuting_money <- aggregated_commuting_money_for_geometry(
  mobility_itineraries_costs_grid |>
    left_join(freguesias |> select(dtmnfr, municipio) |> mutate(dtmnfr = as.integer(dtmnfr)), by = c("Origin_dicofre24" = "dtmnfr")) |>
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
st_write(grid_commuting_money_sf, IMPT_URL(sprintf("%s/grid_commuting_money_car.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(grid_commuting_money, IMPT_URL(sprintf("%s/grid_commuting_money_car.csv", output_dir)), row.names = FALSE)
st_write(freguesia_commuting_money_sf, IMPT_URL(sprintf("%s/freguesia_commuting_money_car.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(freguesia_commuting_money, IMPT_URL(sprintf("%s/freguesia_commuting_money_car.csv", output_dir)), row.names = FALSE)
st_write(municipio_commuting_money_sf, IMPT_URL(sprintf("%s/municipio_commuting_money_car.gpkg", output_dir)), delete_dsn = TRUE)
write.csv(municipio_commuting_money, IMPT_URL(sprintf("%s/municipio_commuting_money_car.csv", output_dir)), row.names = FALSE)
