#testing isochrones for PT stops
# Alternative way of isochrones to PT stops, with time and type of pt

#### RUN AT ushift-delta (ushift-alfa does not handle this) ####

library(stringr)
library(dplyr)
options(java.parameters = "-Xmx96G") # RAM to 16GB
library(r5r)
library(mapview)

# set data path to directory containing your own data if not running this example
# r5r_network <- r5r::build_network("/data/IMPT/geo/r5r/")
r5r_network <- r5r::build_network("data/r5r/")
limit = impt_read("/geo/municipios_union_2024.geojson") |> sf::st_make_valid() # aml limit

# get stops being used
transit_network = transit_network_to_sf(r5r_network)
stops_transit = transit_network$stops
stops_transit = stops_transit |> st_filter(limit) |> select(-linked_to_street) # keep only stops in aml

# classify depending on operator
stops_transit <- stops_transit |> 
  mutate(
    stop_category = case_when(
      # BUS category
      str_detect(stop_id, regex("carris|mobicascais|transportes_colectivos", ignore_case = TRUE)) ~ "BUS",
      # Train & ferry category
      str_detect(stop_id, regex("fertagus|comboios|ttsl|transtejo|soflusa", ignore_case = TRUE)) ~ "Train e ferry",
      # Subway & light rail category
      str_detect(stop_id, regex("metro|mts", ignore_case = TRUE)) ~ "Subway e light rail",
      # Default fallback
      TRUE ~ "Other"
    )
  )
stops_transit <- stops_transit |> 
  mutate(id = as.character(stop_index)) |> 
  select(-stop_index, -stop_id, -stop_name)

mapview(stops_transit, zcol = "stop_category")


### Get isochrones for train/ferry stations using r5r (PRELIMINARY - NOT COMPLETE)

# train and ferry - 15 min
iso_train_ferry <- r5r::isochrone(
  r5r_network,
  origins = stops_transit |> filter(stop_category == "Train e ferry"),
  mode = "WALK",
  cutoffs = 15,
  polygon_output = TRUE, 
  progress = TRUE
)
# mapview(iso_train_ferry)

iso_train_ferry_dissolved <- iso_train_ferry |> 
  st_make_valid() |> 
  summarise(geometry = st_union(polygons)) |> 
  st_make_valid()
mapview(iso_train_ferry_dissolved)

# subway and light rail - 10 min
iso_subway <- r5r::isochrone(
  r5r_network,
  origins = stops_transit |> filter(stop_category == "Subway e light rail"),
  mode = "WALK",
  cutoffs = 10,
  polygon_output = TRUE, 
  progress = TRUE
)
# mapview(iso_subway)

iso_subway_dissolved <- iso_subway |> 
  st_make_valid() |> 
  summarise(geometry = st_union(polygons)) |> 
  st_make_valid()
mapview(iso_subway_dissolved)

# buses - 5 min --- blows up, with 16k stops
# iso_bus <- r5r::isochrone(
#   r5r_network,
#   origins = stops_transit |> filter(stop_category == "BUS"),
#   mode = "WALK",
#   cutoffs = 5,
#   polygon_output = TRUE, 
#   progress = TRUE
# )
# mapview(iso_bus)
# 
# iso_bus_dissolved <- iso_bus |> 
#   summarise(geometry = st_union(polygons)) |> 
#   st_make_valid()
# mapview(iso_bus_dissolved)

# 0. Group bus stops that are 30meters appart, as a single point - it wont affect the results
# this prevents running 2 times for bus stops accross the street

bus_stops <- stops_transit |> filter(stop_category == "BUS")
# 1. Project to Portugal's metric CRS (PT-TM06 / ETRS89)
bus_stops_metric <- st_transform(bus_stops, crs = 3763)

# 2. Buffer by 15m and Dissolve overlapping borders
# Two points 30m apart will have their 15m buffers touch and merge into one polygon
clusters_poly <- st_buffer(bus_stops_metric, dist = 15) |> 
  st_union() |> 
  st_cast("POLYGON") |> 
  st_sf() |> 
  mutate(cluster_id = row_number()) # Give each unique blob an ID

# 3. Spatial Join: Assign the original points to their new cluster IDs
bus_stops_clustered <- st_join(bus_stops_metric, clusters_poly)

# 4. Group by cluster and calculate the mean coordinate (centroid)
bus_stops_consolidated <- bus_stops_clustered |> 
  group_by(cluster_id) |> 
  summarise(
    # Keep track of how many stops merged (good for QA)
    stops_in_cluster = n(), 
    # Combine the points and find their exact geographic center
    geometry = st_centroid(st_union(geometry))
  ) |> 
  # 5. Project back to WGS84 for r5r
  st_transform(crs = 4326)

# Print the results to see how much we reduced the dataset
cat("Original bus stops:", nrow(bus_stops), "\n")
cat("Consolidated stops:", nrow(bus_stops_consolidated), "\n")
cat("Reduction:", round(100 - (nrow(bus_stops_consolidated) / nrow(bus_stops) * 100), 1), "%\n")

# Check the map to verify the new centroids look right
mapview(bus_stops_consolidated)



# 1. Filter out only the BUS stops first
# bus_stops <- stops_transit |> filter(stop_category == "BUS")
bus_stops = bus_stops_consolidated |> rename(id = cluster_id) |> select(-stops_in_cluster)
bus_stops = bus_stops |> filter(!id %in% bad_row_numbers) # not in bad_row_numbers --- see the end of the loop

batch_size <- 400
total_stops <- nrow(bus_stops)
num_batches <- ceiling(total_stops / batch_size)

# Create lists to store both our successes and our error logs
successful_batches <- list()
error_logs <- list()

for (i in 1:num_batches) {
  cat(sprintf("Processing batch %d of %d...\n", i, num_batches))
  
  start_row <- ((i - 1) * batch_size) + 1
  end_row <- min(i * batch_size, total_stops)
  current_batch <- bus_stops |> slice(start_row:end_row)
  
  # Attempt the batch calculation
  result <- tryCatch({
    
    iso_batch <- r5r::isochrone(
      r5r_network,
      origins = current_batch,
      mode = "WALK",
      cutoffs = 5,
      polygon_output = TRUE, 
      progress = TRUE 
    )
    
    # Dissolve immediately to save memory
    batch_dissolved <- iso_batch |> 
      st_make_valid() |> 
      summarise(geometry = st_union(polygons)) |> 
      st_make_valid()
    
    batch_dissolved # This becomes the 'result' if successful
    
  }, error = function(e) {
    # IF IT FAILS: Print a warning and save the exact error message
    cat("  -> [!] Error in batch", i, "- Skipping.\n")
    
    # We use <<- to assign to the list outside the tryCatch environment
    error_logs[[length(error_logs) + 1]] <<- data.frame(
      batch_number = i,
      start_row = start_row,
      end_row = end_row,
      error_message = e$message
    )
    
    return(NULL) # Return NULL so we don't save garbage
  })
  
  # If result is not NULL (meaning it succeeded), save it to our success list
  if (!is.null(result)) {
    successful_batches[[i]] <- result
  }
  
  gc() # Clean memory
}



 # 2. Review the Errors
if (length(error_logs) > 0) {
  cat("\n--- ERROR SUMMARY ---\n")
  error_df <- bind_rows(error_logs)
  print(error_df)
} else {
  cat("\nAll batches completed successfully!\n")
}


#### 3. Review errors
# 1. Extract the problem indices and calculate their exact position
bad_stops_df <- error_df |>
  mutate(
    # Extract the string of numbers using regex (looks for numbers between "point(s): " and ".")
    # e.g., turns "Problem in origin points: 188, 191." into "188, 191"
    extracted_nums = str_match(error_message, "origin points?: ([0-9, ]+)\\.")[,2]
  ) |>
  # Split the comma-separated numbers so each gets its own row
  separate_rows(extracted_nums, sep = ",\\s*") |>
  # Convert to numeric
  mutate(batch_relative_index = as.numeric(extracted_nums))


# 2. Get the unique list of exact row numbers to remove
# bad_row_numbers2 = bad_row_numbers # if need to run again, to store and filter again
bad_row_numbers <- bad_stops_df |> filter(!is.na(batch_relative_index))
bad_row_numbers = unique(bad_row_numbers$batch_relative_index)

cat("Found", length(bad_row_numbers), "problematic stops to remove.\n")

### go back and run again with filtering for bad_row_numbers


# --- POST-PROCESSING ---
# 1. Combine successful batches
cat("\nCombining successful batches...\n")
iso_bus_combined <- bind_rows(successful_batches)

cat("Performing final dissolve...\n")
iso_bus_final <- iso_bus_combined |> 
  st_make_valid() |> 
  summarise(geometry = st_union(geometry)) |> 
  st_make_valid()

mapview(iso_bus_final)




# 4. Combine and Dissolve
# bind_rows is very smart: it automatically ignores all the NULLs in our list!
iso_bus_combined <- bind_rows(iso_list) 

iso_bus_final <- iso_bus_combined |> 
  st_make_valid() |> 
  summarise(geometry = st_union(polygons)) |> 
  st_make_valid()

# Check the final result
mapview(iso_bus_final)




# stop and clean ----------------------------------------------------------

r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)
