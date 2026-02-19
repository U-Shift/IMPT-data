# Load relevant data after preparation

library(dplyr)
library(sf)
library(tidyr)

#Normalizar minmax_norm <- function(x) {
minmax_norm <- function(x) {
  x <- as.numeric(x)
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

invert_01 <- function(x) 1 - x  # para transformar "maior=piores" em "maior=melhor"

# Data location  ----------------------------------------------------------

DATA_LOCATION = "/data/IMPT" # When running at server.ushift.pt, use server local data
#DATA_LOCATION = "https://impt.server.ushift.pt" # When running locally, get data from remote server
# DATA_LOCATION = "data"
API_KEY = Sys.getenv("IMPT_DATA_KEY") # Set it using usethis::edit_r_environ(), followed by CTRL+F10

IMPT_URL = function(path) {
  # If data location starts with "http", add api key to url
  if (startsWith(DATA_LOCATION, "http")) {
    # If API_KEY empty or not defined, throw error
    if (API_KEY == "") {
      stop("IMPT_DATA_KEY env var is not defined. Please set it using usethis::edit_r_environ() and restart R.")
    }
    return(sprintf("%s%s?key=%s", DATA_LOCATION, path, API_KEY))
  }
  # Otherwise, return local path
  return(sprintf("%s%s", DATA_LOCATION, path))
}

readRDS_remote <- function(file, quiet = TRUE) { # From https://stackoverflow.com/a/66874958
  if (grepl("^http", file, ignore.case = TRUE)) {
    # temp location
    file_local <- file.path(tempdir(), basename(file))
    # download the data set
    download.file(file, file_local, quiet = quiet, mode = "wb")
    file <- file_local
  }
  readRDS(file)
}

download_remote_file <- function(dir_url, filename, destinatin_folder) {
  download.file(
    paste0(stringr::str_split(r5r_location, "\\?")[[1]][[1]], filename, "?", stringr::str_split(r5r_location, "\\?")[[1]][[2]]),
    file.path(destinatin_folder, filename),
    mode = "wb"
  )
}

# Geo ---------------------------------------------------------------------

# Polygons administrative
freguesias = st_read(IMPT_URL("/geo/freguesias_2024_unique.gpkg"))
municipios = st_read(IMPT_URL("/geo/municipios_2024.gpkg"))
limit = st_read(IMPT_URL("/geo/municipios_union_2024.geojson"))
limit_bbox = st_read(IMPT_URL("/geo/municipios_union_bbox_2024.geojson"))

# Conversion freguesias
conversion_dicofre = read.csv("useful_data/dicofre_16_24_conversion.csv")
conversion_dicofre_all = readRDS("useful_data/dicofre_16_24_conversion_full.Rds")
conversion_dicofre_weight = readRDS("useful_data/dicofre_16_24_conversion_full_with_weights.Rds")


#-----------------------------------------------------------------------------------
#Accident rates involving cars (taxa anual média)

#TML Data for safety
gpkg_path <- "13-SegurançaRodoviária.gpkg"
layer_cars <- "ANSR_AML_sinistralidade - VEÍCULO - Ligeiros"

#Checking freguesias
freguesias_sf <- st_make_valid(freguesias)

#Read Data for accidents involving car
car <- st_read(gpkg_path, layer = layer_cars) %>% st_transform(st_crs(freguesias_sf))

#Count number of accidents per year per freguesia
acc_freg_ano <- st_join(car, freguesias_sf["dtmnfr"], left = FALSE) %>%
  st_drop_geometry() %>%
  count(dtmnfr, Ano, name = "n_car")

#Complete freguesias with zeros
anos <- sort(unique(acc_freg_ano$Ano))
acc_freg_ano <- acc_freg_ano %>%
  complete(dtmnfr = unique(freguesias_sf$dtmnfr), Ano = anos, fill = list(n_car = 0))

# Taxa anual média por freguesia
taxa_anual_media_freguesia <- acc_freg_ano %>%
  group_by(Ano) %>%
  mutate(
    total_ano = sum(n_car),
    share_ano = ifelse(total_ano > 0, n_car / total_ano, 0)
  ) %>%                      # <-- ESTE %>% estava a faltar
  ungroup() %>%
  group_by(dtmnfr) %>%
  summarise(
    taxa_anual_media = mean(share_ano, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    freguesias_sf %>% st_drop_geometry() %>% distinct(dtmnfr, freguesia, municipio),
    by = "dtmnfr"
  ) %>%
  select(dtmnfr, freguesia, municipio, taxa_anual_media) %>%
  arrange(desc(taxa_anual_media))

#Aplying min-max to taxa anual média de acidentes envolvendo carros por freguesia
taxa_anual_media_freguesia <- taxa_anual_media_freguesia %>%
  mutate(
    taxa_anual_media_norm  = minmax_norm(taxa_anual_media),
    taxa_anual_media_score = invert_01(taxa_anual_media_norm)
  )
  
  # ------------------------
  # Checks + Top 10
  # ------------------------
  summary(taxa_anual_media_freguesia$taxa_anual_media)
  summary(taxa_anual_media_freguesia$taxa_anual_media_norm)
  summary(taxa_anual_media_freguesia$taxa_anual_media_score)
  
  taxa_anual_media_freguesia %>%
    arrange(desc(taxa_anual_media)) %>%
    select(dtmnfr, freguesia, municipio, taxa_anual_media, taxa_anual_media_norm, taxa_anual_media_score) %>%
    head(10)
  
#------------------------------------------------------------------------------------------

#Pedestrian accident rates (taxa anual média)
  
  layer_ped <- "ANSR_AML_sinistralidade - ATROPELAMENTOS"
  
  # Read pedestrian accident points and align CRS
  ped <- st_read(gpkg_path, layer = layer_ped) %>%
    st_transform(st_crs(freguesias_sf))
  
  # Spatially assign each pedestrian accident to a parish (dtmnfr)
  ped_w <- st_join(ped, freguesias_sf["dtmnfr"], left = FALSE)
  
  # Count pedestrian accidents by parish-year
  ped_by_parish_year <- ped_w %>%
    st_drop_geometry() %>%
    count(dtmnfr, Ano, name = "n_ped")
  
  # Years present in the dataset
  years <- sort(unique(ped_by_parish_year$Ano))
  
  # Restrict to parishes that actually appear in the pedestrian dataset (AML-only)
  aml_parishes <- ped_w %>%
    st_drop_geometry() %>%
    distinct(dtmnfr)
  
  # Complete missing parish-year combinations with zeros
  ped_by_parish_year <- ped_by_parish_year %>%
    complete(
      dtmnfr = aml_parishes$dtmnfr,
      Ano    = years,
      fill   = list(n_ped = 0)
    )
  
  # Compute yearly share per parish, then average across years
  ped_avg_annual_share_parish <- ped_by_parish_year %>%
    group_by(Ano) %>%
    mutate(
      total_year = sum(n_ped),
      share_year = ifelse(total_year > 0, n_ped / total_year, 0)
    ) %>%
    ungroup() %>%
    group_by(dtmnfr) %>%
    summarise(avg_annual_share_ped = mean(share_year), .groups = "drop") %>%
    left_join(
      freguesias_sf %>% st_drop_geometry() %>% distinct(dtmnfr, freguesia, municipio),
      by = "dtmnfr"
    ) %>%
    mutate(
      # 0-1 normalization of the average annual share (higher = worse)
      avg_annual_share_ped_norm = minmax_norm(avg_annual_share_ped),
      
      # Optional: inverted score so that higher = better (lower accident share)
      avg_annual_share_ped_score = invert_01(avg_annual_share_ped_norm)
    ) %>%
    select(
      dtmnfr, freguesia, municipio,
      avg_annual_share_ped,
      avg_annual_share_ped_norm,
      avg_annual_share_ped_score
    ) %>%
    arrange(desc(avg_annual_share_ped))
  
  # Final result
  ped_avg_annual_share_parish
  
#------------------------------------------------------------------------------------------

#Cyclist accident rates (taxa anual média)
  
  layer_bike <- "ANSR_AML_sinistralidade - VEÍCULO - Velocipedes"
  
  # Read bicycle accident points and align CRS
  bike <- st_read(gpkg_path, layer = layer_bike) %>%
    st_transform(st_crs(freguesias_sf))
  
  # Spatially assign each bicycle accident to a parish (dtmnfr)
  bike_w <- st_join(bike, freguesias_sf["dtmnfr"], left = FALSE)
  
  # Count bicycle accidents by parish-year
  bike_by_parish_year <- bike_w %>%
    st_drop_geometry() %>%
    count(dtmnfr, Ano, name = "n_bike")
  
  # Years present in the dataset
  years <- sort(unique(bike_by_parish_year$Ano))
  
  # Restrict to parishes that appear in the bicycle dataset (AML-only)
  aml_parishes <- bike_w %>%
    st_drop_geometry() %>%
    distinct(dtmnfr)
  
  # Complete missing parish-year combinations with zeros
  bike_by_parish_year <- bike_by_parish_year %>%
    complete(
      dtmnfr = aml_parishes$dtmnfr,
      Ano    = years,
      fill   = list(n_bike = 0)
    )
  
  # Compute yearly share per parish, then average across years, then normalize
  bike_avg_annual_share_parish <- bike_by_parish_year %>%
    group_by(Ano) %>%
    mutate(
      total_year = sum(n_bike),
      share_year = ifelse(total_year > 0, n_bike / total_year, 0)
    ) %>%
    ungroup() %>%
    group_by(dtmnfr) %>%
    summarise(avg_annual_share_bike = mean(share_year), .groups = "drop") %>%
    left_join(
      freguesias_sf %>% st_drop_geometry() %>% distinct(dtmnfr, freguesia, municipio),
      by = "dtmnfr"
    ) %>%
    mutate(
      # 0-1 normalization of the average annual share (higher = worse)
      avg_annual_share_bike_norm  = minmax_norm(avg_annual_share_bike),
      
      # Optional: inverted score so that higher = better (lower accident share)
      avg_annual_share_bike_score = invert_01(avg_annual_share_bike_norm)
    ) %>%
    select(
      dtmnfr, freguesia, municipio,
      avg_annual_share_bike,
      avg_annual_share_bike_norm,
      avg_annual_share_bike_score
    ) %>%
    arrange(desc(avg_annual_share_bike))
  
  # Final result
  bike_avg_annual_share_parish  
  
#------------------------------------------------------------------------------------------
  
#
  
  