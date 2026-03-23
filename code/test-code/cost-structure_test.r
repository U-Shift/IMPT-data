# Define cost structure for AML, with 2 scenarios (per trip or woth PT pass)
# https://ipeagit.github.io/r5r/reference/setup_fare_structure.html
# https://u-shift.github.io/Traffic-Simulation-Models/pareto.html
# https://ipeagit.github.io/r5r/articles/fare_structure.html

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
options(java.parameters = "-Xmx64G") # RAM to 16GB
library(r5r)
library(dplyr)
library(sf)
library(tidyr)

# Load r5r Lisbon data, that includes all the GTFS
r5r_lisboa <- r5r::build_network(data_path = "data/r5r/")
transit_net <- transit_network_to_sf(r5r_lisboa)


### Scenario A: Single tickets (Prepaid/Zapping)
# Based on the data, prepaid tickets cost ~1.72€ and allow 60-min transfers limits.
# We'll set 1.72€ as the base public transport fare with no added cost for transfers.
fare_single <- setup_fare_structure(r5r_lisboa,
    base_fare = 1.72,
    by = "MODE"
)

# Transfers within PT are included in the 60 min window (0 extra cost)
fare_single$fares_per_transfer <- fare_single$fares_per_transfer |>
    mutate(fare = 0) # No extra cost for transferring

# Set transfer time to 60 minutes (this matches Zapping for Carris & Metro)
fare_single$transfer_time_allowance <- 60

fare_single$fares_per_type <- fare_single$fares_per_type |>
    mutate(
        unlimited_transfers = TRUE,
        allow_same_route_transfer = TRUE
    )


### Scenario B: Monthly Pass
# For monthly pass holders, calculating the apportioned average cost per journey.
# Assuming a 40€ pass and an average of ~44 trips per month, the apportioned 
# cost per journey is approximately 0.90€. We will use this as the base fare.
fare_pass <- setup_fare_structure(r5r_lisboa,
    base_fare = 0.90,
    by = "MODE"
)

# Transfers within the journey are considered part of the same 0.90€ average cost
fare_pass$fares_per_transfer <- fare_pass$fares_per_transfer |>
    mutate(fare = 0)

fare_pass$transfer_time_allowance <- 120 # Setting a generous transfer time (e.g., 2 hours)

fare_pass$fares_per_type <- fare_pass$fares_per_type |>
    mutate(
        unlimited_transfers = TRUE,
        allow_same_route_transfer = TRUE
    )


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
#       mode == "BICYCLE" ~ 1,      # 1€ flat fee (e.g. GIRA bikeshare)
#       mode == "WALK" ~ 0.5,       # 0.5€ flat cost (value of effort/shoe wear)
#       TRUE ~ as.numeric(fare)     # for PT modes, r5r will calculate the fare
#     )
#   )
