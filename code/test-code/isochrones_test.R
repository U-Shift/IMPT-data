#testing isochrones for PT stops
# Alternative way of isochrones to PT stops, with time and type of pt

library(stringr)
library(dplyr)
options(java.parameters = "-Xmx32G") # RAM to 16GB
library(r5r)
# set data path to directory containing your own data if not running this example
r5r_network <- r5r::build_network("/data/IMPT/geo/r5r/")
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
mapview(iso_train_ferry)

iso_train_ferry_dissolved <- iso_train_ferry |> 
  summarise(geometry = st_union(polygons)) |> 
  st_make_valid()
mapview(iso_train_ferry_dissolved)

# subway and light rail - 10 min
iso_train_subway <- r5r::isochrone(
  r5r_network,
  origins = stops_transit |> filter(stop_category == "Subway e light rail"),
  mode = "WALK",
  cutoffs = 10,
  polygon_output = TRUE, 
  progress = TRUE
)
mapview(iso_train_subway)

iso_train_subway_dissolved <- iso_train_subway |> 
  summarise(geometry = st_union(polygons)) |> 
  st_make_valid()
mapview(iso_train_subway_dissolved)
