# Define cost structure for AML, with 2 scenarios (per trip or woth PT pass)
# https://ipeagit.github.io/r5r/reference/setup_fare_structure.html
# https://u-shift.github.io/Traffic-Simulation-Models/pareto.html
# Detailed docs (very useful!) https://ipeagit.github.io/r5r/articles/fare_structure.html

# Use the real single fare costs (carris, metro, cm, fluvial), instead of the PT monthly pass.
# Use 8€ for car trips (uber?)
# Use 1€ for walk.

# When using pass, 40€ per month ofr all AML, 30€ in Lisbon

# From Carris Lisbon (Bus system Lisbon city)[https://www.carris.pt/media/ikvdezmc/relat%C3%B3rio-e-contas-carris-2024.pdf]:
# In 2024, there were 116480000 validations with PT pass, and 17424000 single ticket validations.
# They sold 43256000 € in monthly passes (30 days), and 33278 € in single tickets.
# 10,7% of the € revenue were from on-board single tickets (2.30€) and the remaining were prepaid (1.72 to 1.90€).
# prepaid single tickets are valie for 1 hour (https://www.carris.pt/media/hlndnvcp/a4_tarifario_vertical_site.pdf), but on-board are valid only for 1 ride (no transfers)

# From Carris Metropolitana (Bus system metro region)[https://www.tmlmobilidade.pt/wp-content/uploads/2021/12/Relatorio-e-Contas-2024.pdf]
# PT pass sold - 254582000 € in 2024
# Single ticket sold - 136842 € in 2024

# PT pass metropolitan (40€) - 5063510 anual recharges
# PT pass municipal (30€) - 887190 anual recharges
# PT pass senior (20€) - 1103782 anual recharges
# PT pass family (80€) - 243784 anual recharges
# PT pass young sub23 (0€) - 3425013 anual recharges

# Number of validations in 2025
# urban trains (Fertagus, CP, MTS) - 117376000
# Buses (carris metropolitana, Carris Lisboa, MobiCascais, TCBarreiro, etc) - 326179000
# Ferry - 20814000
# Subway (Metropolitano de Lisboa) - 189686000

# the single trips at subway (metropolitano de lisboa) cost between 1.72€ and 1.90€ with unlimited transfers for 60min.
# https://www.metrolisboa.pt/wp-content/uploads/2025/12/Tarifas_site_2026_PT.pdf

# For car trips, we can use 0.39€/km (Belga, 2021) + parking cost (2€/h) + tolls when needed (entreing Lisbon costs 2€)

# For walk trips, we can use 0.5€/trip

# For bike trips, we can use 1€/trip


##### R
options(java.parameters = "-Xmx96G") # RAM to 16GB
library(r5r)
library(dplyr)
library(sf)
library(tidyr)

# Load r5r Lisbon data, that includes all the GTFS
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

# transit_net <- transit_network_to_sf(r5r_network)


### Scenario A: Single tickets (Prepaid/On-board)
# Calculation based on Carris Lisbon Data:
# - Total single ticket validations = 17,424,000
# - Total single ticket revenue = 33,278(k) € = 33,278,000 €
# - Average cost per single ticket = 33,278,000 / 17,424,000 = ~1.91 €
# (While Zapping is 1.72-1.90, blending the 10.7% on-board revenue at 2.30€ puts the overall average at 1.91€)
fare_single <- setup_fare_structure(r5r_network,
    base_fare = 1.91,
    by = "MODE"
)

fare_single$max_discounted_transfers <- 0 # No discounted transfers, as single tickets are only valid for 1 ride (no transfers allowed)

# Transfers within PT are included in the 60 min window (0 extra cost)
fare_single$fares_per_transfer <- fare_single$fares_per_transfer |>
    mutate(fare = case_when(first_leg==second_leg ~ 0, TRUE ~ 1.91)) # No extra cost for transferring

# Set transfer time to 60 minutes
fare_single$transfer_time_allowance <- 60

fare_single$fares_per_type <- fare_single$fares_per_type |>
    mutate(
      unlimited_transfers = TRUE, # case_when(type %in% c("SUBWAY", "TRAM") ~ TRUE, TRUE ~ FALSE), # Allow unlimited transfers for subway and tram, but not for bus and ferry, to reflect the fact that bus single tickets are only valid for 1 ride, while subway/tram are valid for 60min with unlimited transfers.
      allow_same_route_transfer = TRUE
    )

# save fare rules to file
r5r::write_fare_structure(fare_single, file_path = IMPT_URL("geo/r5r/fares_single.zip"))
fare_stucture_single_file = tempfile(fileext = ".zip")
download.file(IMPT_URL("geo/r5r/fares_single.zip"), fare_stucture_single_file, mode = "wb")
fare_single <- r5r::read_fare_structure(fare_stucture_single_file)


### Scenario B: Monthly Pass
# Calculation based on operator data:
# 1. Total Pass Revenue: (5063510 * 40€) + (887190 * 30€) + (1103782 * 20€) + (243784 * 80€) + (3425013 * 0€) = 270,734,460 €
# 2. Total Pass Recharges (months): 10,723,279
# 3. Average Paid per Pass Month: 270,734,460 € / 10,723,279 = ~25.25 €
# 4. Approximating 44 journeys/month (2 trips a day * 22 working days), the cost per journey = 25.25 € / 44 = ~0.57 €
# (Note: the 3.4M free young passes significantly decrease the base average cost paid per user).
fare_pass <- setup_fare_structure(r5r_network,
    base_fare = 0.57,
    by = "MODE"
)

fare_pass$fare_cap = 0.57 # Monthly pass

# Transfers within the journey are considered part of the same 0.57€ average cost
fare_pass$fares_per_transfer <- fare_pass$fares_per_transfer |>
    mutate(fare = 0.57)

fare_pass$transfer_time_allowance <- 120 # Setting a generous transfer time (e.g., 2 hours)

fare_pass$fares_per_type <- fare_pass$fares_per_type |>
    mutate(
        unlimited_transfers = TRUE,
        allow_same_route_transfer = TRUE
    )

# save fare rules to file
r5r::write_fare_structure(fare_pass, file_path = IMPT_URL("geo/r5r/fares_pass.zip"))
fare_stucture_pass_file = tempfile(fileext = ".zip")
download.file(IMPT_URL("geo/r5r/fares_pass.zip"), fare_stucture_pass_file, mode = "wb")
fare_pass <- r5r::read_fare_structure(fare_stucture_pass_file)

### Note on Non-PT mode costs (Car, Bike, Walk)
# r5r's setup_fare_structure only applies to transit (PT) legs.
# To compute point-to-point monetary costs for Car, Bike, and Walk, you calculate them
# analytically AFTER routing based on distance and duration.
#
# EXAMPLE using detailed_itineraries():
#
# itineraries <- detailed_itineraries(...)
# itineraries_with_cost <- itineraries |>
#   mutate(
#     monetary_cost = case_when(
#       mode == "CAR" ~ (distance / 1000) * 0.39 + (duration / 60) * 2 + 2, # distance(km) * 0.39€ + time(h) * 2€ + 2€ toll
#       mode == "BICYCLE" ~ 0.25,      # 0.25€ flat fee (e.g. GIRA bikeshare, maintnance)
#       mode == "WALK" ~ 0.1,       # 0.1€ flat cost (value of effort/shoe wear)
#       TRUE ~ as.numeric(fare)     # for PT modes, r5r will calculate the fare
#     )
#   )
