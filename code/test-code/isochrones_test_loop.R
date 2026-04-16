

# --- 1. THE DIVIDE AND CONQUER FUNCTION ---
# This function calls itself, splitting the data in half every time it hits an error.
process_with_split <- function(chunk, depth = 0) {
  n <- nrow(chunk)
  indent <- paste(rep("  ", depth), collapse = "") 
  
  caught_error <- NULL
  
  # ATTEMPT: Try the current chunk
  result <- tryCatch({
    iso <- r5r::isochrone(
      r5r_network,
      origins = chunk,
      mode = "WALK",
      cutoffs = 5,
      polygon_output = TRUE, 
      progress = FALSE 
    )
    
    # THE FIX: st_union() automatically finds the active geometry column.
    # Then st_sf() forces the final column to be named 'geometry'.
    iso_valid <- st_make_valid(iso)
    unioned_geom <- st_union(iso_valid) |> st_make_valid()
    st_sf(geometry = unioned_geom)
    
  }, error = function(e) {
    caught_error <<- e$message 
    return(NULL) 
  })
  
  # SCENARIO A: It succeeded!
  if (!is.null(result)) {
    cat(sprintf("%s[✓] Success! Processed chunk of %d stops.\n", indent, n))
    return(result)
  }
  
  # SCENARIO B: It failed, and there is only 1 stop left. 
  if (n == 1) {
    cat(sprintf("%s[X] Broken stop dropped. Reason: %s\n", indent, caught_error))
    return(NULL)
  }
  
  # SCENARIO C: It failed, and we have multiple stops. SPLIT IN HALF!
  cat(sprintf("%s[!] Chunk of %d failed. Splitting...\n", indent, n))
  mid <- floor(n / 2)
  chunk1 <- chunk |> slice(1:mid)
  chunk2 <- chunk |> slice((mid + 1):n)
  
  res1 <- process_with_split(chunk1, depth + 1)
  res2 <- process_with_split(chunk2, depth + 1)
  
  # Combine whatever good data we salvaged from the halves
  salvaged <- bind_rows(res1, res2)
  
  if (nrow(salvaged) > 0) {
    # THE FIX applied to the salvaged combine step as well
    salvaged_valid <- st_make_valid(salvaged)
    salvaged_geom <- st_union(salvaged_valid)
    return(st_sf(geometry = salvaged_geom))
  } else {
    return(NULL)
  }
}

# --- 2. THE MAIN LOOP ---
# Assuming you are using your 'bus_stops_consolidated' dataset
stops_to_process <- bus_stops #clean and consolidated, 10491 

batch_size <- 400
total_stops <- nrow(stops_to_process)
num_batches <- ceiling(total_stops / batch_size)

successful_results <- list()

for (i in 1:num_batches) {
  cat(sprintf("\n--- Processing Main Batch %d of %d ---\n", i, num_batches))
  
  start_row <- ((i - 1) * batch_size) + 1
  end_row <- min(i * batch_size, total_stops)
  current_batch <- stops_to_process |> slice(start_row:end_row)
  
  # We pass the batch to our smart recursive function. 
  # If the 400 passes, it returns immediately. If it fails, it handles the splitting!
  batch_result <- process_with_split(current_batch, depth = 1)
  
  if (!is.null(batch_result)) {
    successful_results[[i]] <- batch_result
  }
  
  gc() # Clean memory after every major batch
}

# --- 3. FINAL CONSOLIDATION ---
cat("\nAll batches finished! Combining everything...\n")
iso_bus_final <- bind_rows(successful_results) |> 
  summarise(geometry = st_union(geometry)) |> 
  st_make_valid()

# Look at your final map!
library(mapview)
mapview(iso_bus_final)