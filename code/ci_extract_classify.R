# 02b_ci_osmactive.R
# Extract historical cycling network infrastructure using osmactive and custom OSM tags logic

library(osmactive)
library(sf)
library(dplyr)
sf_use_s2(TRUE)
options(timeout = 3600)



# Functions' definition ----------------------------------------------------


# Custom ci classification logic ported from 02_ci_osmextract_custom.R (eugenevidal)
cycleway_cols <- function(x) { names(x)[grepl("^cycleway($|[:_])", names(x), ignore.case = TRUE)] }
has_cycleway_vals <- function(x, vals) {
  cols <- cycleway_cols(x)
  if (!length(cols)) return(rep(FALSE, nrow(x)))
  vals <- tolower(vals)
  out  <- rep(FALSE, nrow(x))
  for (cc in cols) {
    v <- tolower(trimws(as.character(x[[cc]])))
    v[is.na(v)] <- ""
    hit <- vapply(strsplit(v, ";", fixed = TRUE), function(parts) any(trimws(parts) %in% vals), logical(1))
    out <- out | hit
  }
  out
}

STRONG_ONROAD_VALS <- c("track", "opposite_track")
MODERATE_ONROAD_VALS <- c("lane", "opposite_lane")
WEAK_ONROAD_VALS <- c("share_busway", "shared_lane")
FOOT_SHARED_HWY  <- c("path", "footway", "pedestrian")
FOOT_SHARED_BIC  <- c("yes", "designated")
FOOT_SHARED_FOOT <- c("yes", "designated")
PAVEMENT_LANE_HWY <- c("footway", "path", "pedestrian")
PAVEMENT_LANE_BIC <- c("designated")

classify_custom_ci <- function(lines_m) {
  highway    <- tolower(trimws(as.character(lines_m$highway)))
  if ("bicycle" %in% names(lines_m)) bicycle <- tolower(trimws(as.character(lines_m$bicycle))) else bicycle <- rep(NA_character_, nrow(lines_m))
  if ("foot" %in% names(lines_m)) foot <- tolower(trimws(as.character(lines_m$foot))) else foot <- rep(NA_character_, nrow(lines_m))
  if ("segregated" %in% names(lines_m)) segregated <- tolower(trimws(as.character(lines_m$segregated))) else segregated <- rep(NA_character_, nrow(lines_m))
  
  is_cyclewy <- !is.na(highway) & highway == "cycleway"
  
  has_strong_onroad   <- has_cycleway_vals(lines_m, STRONG_ONROAD_VALS)
  has_moderate_onroad <- has_cycleway_vals(lines_m, MODERATE_ONROAD_VALS)
  has_weak_onroad     <- has_cycleway_vals(lines_m, WEAK_ONROAD_VALS)
  
  is_foot_shared <- (!is.na(highway) & highway %in% FOOT_SHARED_HWY) &
    (!is.na(bicycle) & bicycle %in% FOOT_SHARED_BIC) &
    (!is.na(foot) & foot %in% FOOT_SHARED_FOOT) &
    !(segregated %in% "yes")
  
  is_pavement_lane <- (!is.na(highway) & highway %in% PAVEMENT_LANE_HWY) &
    (!is.na(bicycle) & bicycle %in% PAVEMENT_LANE_BIC) &
    (TRUE | !(segregated %in% "yes"))
  
  lines_m$cycle_cat <- NA_character_
  lines_m$cycle_cat[is_cyclewy] <- "strong_ci"
  
  sel_foot <- (is_foot_shared | is_pavement_lane) & is.na(lines_m$cycle_cat)
  lines_m$cycle_cat[sel_foot] <- "shared_foot"
  
  sel_other <- is.na(lines_m$cycle_cat)
  lines_m$cycle_cat[sel_other & has_strong_onroad] <- "strong_ci"
  
  sel_other <- is.na(lines_m$cycle_cat)
  lines_m$cycle_cat[sel_other & has_moderate_onroad] <- "moderate_ci"
  
  sel_other <- is.na(lines_m$cycle_cat)
  lines_m$cycle_cat[sel_other & has_weak_onroad] <- "weak_ci"
  
  lines_m <- lines_m[!is.na(lines_m$cycle_cat), , drop = FALSE]
  
  lines_m$infra5 <- factor(lines_m$cycle_cat, 
                           levels = c("strong_ci", "moderate_ci", "weak_ci", "shared_foot"),
                           # labels = c("Separated cycling infrastructure", "Painted on-road cycle lane", "Mixed traffic (motor vehicles with light infra)", "Cycling on pedestrian infrastructure")
                           labels = c("Ciclovia segregada", "Ciclofaixa pintada na via", "Via partilhada com veículos motorizados (sinalizada)", "Infraestrutura pedonal partilhada") # PT
  )
  return(lines_m)
}



# Run for a area ----------------------------------------------------------




#### Configuration - change here

region_map <- list(
  AML = "portugal")

city = "AML"
limit = "path_to_polygon.gpkg" # Caminho onde está o polígono da AML .gpkg
out_path = "path_to_export_ci_osmactive_.gpkg" # Caminho onde queres exportar o resultado

  
  perim_path <- limit
  perim <- sf::st_read(perim_path, quiet = TRUE) |> sf::st_make_valid()
  if (sf::st_crs(perim)$epsg != 4326) perim <- sf::st_transform(perim, 4326)
  
  infra_region <- region_map[[city]]
  
    
    cat(paste("Extracting CI for", city, "...\n"))
    
    tryCatch(
      {
        osm <- osmactive::get_travel_network(
          place = infra_region,
          boundary = perim,
          boundary_type = "clipsrc",
          # download_directory = osm_raw_dir, # se quiseres definir um sítio para guardar o osm.pbf original (muito grande)
          quiet = FALSE
        )
        
        cycle_net <- osmactive::get_cycling_network(osm)
        
        cycle_net <- classify_custom_ci(cycle_net)
        
        cycle_net <- cycle_net |>
          sf::st_transform(4326)
        
        sf::st_write(cycle_net, out_path, driver = "GPKG", append = FALSE, quiet = TRUE)
        cat(paste("Successfully saved", out_path, "\n"))
      },
      error = function(cond) {
        warning(paste("Failed to process", city, ":", cond$message))
      }
    )
    
    if (exists("osm")) rm(osm)
    if (exists("cycle_net")) rm(cycle_net)
    gc()
    

cat("Cycling infrastructure extraction and classification finished.\n")