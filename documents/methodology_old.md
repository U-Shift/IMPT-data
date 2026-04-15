# Data preparation before indicators

include the geo data preparation and sources,
jittering,
pois,
update from 2011/2016/2021 to 2024 administrative areas

---

# 🤝 Accesibility

# Accessibility

## Geographical unit

We [have decided](https://github.com/U-Shift/IMPT-data/issues/5) to compute accessibility measures using the [H3 grid](https://h3geo.org/docs/core-library/restable) with medium resolution 8 (edge length of 531 m, average hexagon area of 737,327.598 m²).

This corresponds to 3686 hexagons in the Lisbon Metropolitan Area.

## Data

### POIs

Code at [https://github.com/U-Shift/IMPT-data/blob/main/code/data_prep.R\#L437](https://github.com/U-Shift/IMPT-data/blob/main/code/data_prep.R#L437)

We have used several datasets for Points of Interest:

- OSM: OpenStreetMap database for 04/2024 (osm_poi_landuse.gpkg at [github.com/U-Shift/SiteSelection](https://github.com/U-Shift/SiteSelection/releases/tag/0.1))

- CM: Carris Metropolitana open datasets ([github.com/carrismetropolitana/datasets](https://github.com/carrismetropolitana/datasets))

- IMOB 2017/18

- GTFS data for all transit operators for 04/02/2026

Below are the details for each category.

| Category     | Data source                                                                                                    | Filters applied                                                                                                                                                                                        | Identifier (Eg. access_health_car_60min_residents) |
| :----------- | :------------------------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :------------------------------------------------- |
| Healthcare   | [CM](https://github.com/carrismetropolitana/datasets/blob/latest/facilities/health_centers/health_centers.csv) | Aggregated analysis but also individual for _Hospitals_ and _Primary Care_ facilities                                                                                                                  | `healthhealth_primary health_hospital`             |
| Supermarket  | OSM                                                                                                            | _type %in% c("supermarket", "convenience")_                                                                                                                                                            | `groceries`                                        |
| Green spaces | OSM                                                                                                            | _group \== "leisure" & type %in% c("park", "garden")_                                                                                                                                                  | `greenspaces`                                      |
| Recreation   | OSM                                                                                                            | _type %in% c("library", "theatre", "cinema", "restaurant", "cafe", "bar", "pub","pitch", "fitness_station", "swimming_pool", "sports_centre", "fitness_center")_                                       | `recreation`                                       |
| Schools      | [CM](https://github.com/carrismetropolitana/datasets/raw/refs/heads/latest/facilities/schools/schools.csv)     | Aggregated analysis but also individual for primary schools (1st to 3rd cycle, [reference](https://en.wikipedia.org/wiki/Education_in_Portugal#Years_of_schooling))                                    | `schools schools_primary`                          |
| Jobs         | IMOB 2017                                                                                                      | [Jittering](https://github.com/itsleeds/odjitter) with trip survey data from IMOB 2017 (details on jittering below)                                                                                    | `jobs`                                             |
| Transit      | GTFS                                                                                                           | Aggregated analysis with all stops of all operators but also individual analysis for bus (Carris, Carris Metropolitana, TCB, Cascais próxima) and mass transit (CP, Fertagus, Metro Lisboa, MTS, TTSL) | `transit transit_bus transit_mass`                 |

#### Jittering

Code at [https://github.com/U-Shift/IMPT-data/blob/main/code/jobs_imob.R](https://github.com/U-Shift/IMPT-data/blob/main/code/jobs_imob.R)

IMOB travel survey provided us with OD matrix at parish level, representing a “centroid to centroid representation, a common approach involving the simplifying assumption that all trip destinations and origins can be represented by (sometimes population weighted or aggregated) zone centroids” ([Lovelace et al., 2022](https://doi.org/10.32866/001c.33873)) However, to compute accessibility and mobility measures, we needed more disaggregated locations of job within the parish area.

For this purpose, we have used jittering as a proxy, adjusting start and end locations weighted on volume of buildings acting as trip attractors, disaggregating trips between parishes in ODs with up to 50 trips.

“OD pairs are sampled, and weights representing the importance of different trip ‘originators’ and ‘attractors” ([Lovelace et al., 2022](https://doi.org/10.32866/001c.33873))

### Census data

Code at [https://github.com/U-Shift/IMPT-data/blob/main/code/data_prep.R\#L403](https://github.com/U-Shift/IMPT-data/blob/main/code/data_prep.R#L403)

We are considering Census data for 2021, at the BGRI level (using centroids), using total number of residents and age groups for some dimensions, according to the table below.

| Variable name (Eg. access_health_car_60min_residents) | Description                                                    |
| :---------------------------------------------------- | :------------------------------------------------------------- |
| `residents`                                           | Total number of people of all ages (INE variable N_INDIVIDUOS) |
| `kids`                                                | Ages 65 or \+ (INE variable N_INDIVIDUOS_65_OU_MAIS)           |
| `active`                                              | Ages 25 to 64 (INE variable N_INDIVIDUOS_25_64)                |
| `elder`                                               | Ages 0 to 14 (INE variable N_INDIVIDUOS_0_14)                  |

## Methodology

### Travel time matrix

Code at [https://github.com/U-Shift/IMPT-data/blob/main/code/ttm_gridh3.R](https://github.com/U-Shift/IMPT-data/blob/main/code/ttm_gridh3.R)

All accessibility measures are based on a travel matrix, computed between each grid cell over a [network](https://ipeagit.github.io/r5r/reference/build_network.html) that was built considering:

- OSM road network exported from [HOT Export Tool](https://export.hotosm.org/exports/4782f0b8-6778-4c0e-8e4f-97fc62e7f240)
- GTFs transit feed for all operators (valid at 04/02/2026)
- Terrain elevation profile, obtained from [Copernicus](https://browser.dataspace.copernicus.eu/)

The matrix was computed using [r5r::travel_time_matrix](https://ipeagit.github.io/r5r/reference/travel_time_matrix.html), considering the following parameters (parameters non mentioned below have been considered with the default values):

- For all modes
  - percentiles \= 50
    - Percentile to use when returning travel time estimates
  - max_trip_duration \= 120
    - The maximum trip duration in minutes. Cutoff.
- For public transit
  - mode_egress=WALK
    - Transport mode used after egress from the last public transport
  - max_walk_time \= 15
    - The maximum walking time (in minutes) to access and egress the transit network, to make transfers within the network
  - departure_datetime \= 04/02/2026 at 08:00:00 AM
  - Max_rides \= 2 (1 transfer) and 3 (2 transfers)
    - The maximum number of public transport rides allowed in the same trip
- For cycling
  - max_lts \= 3
    - The maximum level of traffic stress that cyclists will tolerate ([\+ details](https://ipeagit.github.io/r5r/reference/travel_time_matrix.html#level-of-traffic-stress-lts-))

### Number of opportunities reachable within threshold (travel time)

Code at [code/access_opportunities.R](https://github.com/U-Shift/IMPT-data/blob/main/code/access_opportunities.R)

Flow chart at [code/flow_charts/accessibility.png](https://github.com/U-Shift/IMPT-data/blob/main/code/flow_charts/accessibility.png)

To compute access to opportunities, we have used [accessibility::cumulative_cutoff](https://ipeagit.github.io/accessibility/reference/cumulative_cutoff.html) considering:

- The travel matrixes previously computed for each mode for each grid cell combination;
- The number of opportunities in each grid cell (grouped by grid cell, summed);
- Cut offs depending on the mode/POI category combination ([Data Index](https://docs.google.com/spreadsheets/d/1BXOk0h19cJWYC9n_FeZW0BoNSBSc6SxVU61_jtpy4LE/edit?gid=0#gid=0)):
  - We computed all the modes and POI categories for cut offs of 5,10,15,30,45,60,75,90 minutes. The ones that meet the defined thresholds in the spread should be selected.

The value computed represents the number of opportunities reachable within the time threshold defined. Values at parish and municipality level are aggregated considering a weighted mean of the number of opportunities reachable in each grid cell within that geographical unit, weighted by the population of those cells.

#### Results

##### Data tables

`freguesia_accessibility = st_read(IMPT_URL("/accessibility/r8/accessibility_freguesia.gpkg"))`  
`municipio_accessibility = st_read(IMPT_URL("/accessibility/r8/accessibility_municipio.gpkg"))`  
`grid_accessibility = st_read(IMPT_URL("/accessibility/r8/accessibility_grid.gpkg"))`

##### Data structure (columns)

| Column                                                                        | Description                                                                                         | Example                           |
| :---------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------------------- | :-------------------------------- |
| `Id dtmnfr municipio`                                                         | Geographical unit ID for grid Geographical unit ID for parish Geographical unit ID for municipality | 1 110501 Alcochete                |
| `buildings                                     families kids elder residents` | Census data                                                                                         |                                   |
| `n_<POI>`                                                                     | Number of POIs for that category                                                                    | n_health                          |
| `access_<POI>_<mode>_<time>_<census>`                                         | Access to POI category using mode with a cut off time for a category of census population           | access_health_bike_5min_residents |

### Travel time to first (1,2,3) opportunities

Code at [code/mobility_costs_time.R](https://github.com/U-Shift/IMPT-data/blob/main/code/mobility_costs_time.R)

Flow chart at [code/flow_charts/mobility_commuting_travel_time_peak.png](https://github.com/U-Shift/IMPT-data/blob/main/code/flow_charts/mobility_commuting_travel_time_peak.png)

Computed using [accessibility::cost_to_closest](https://ipeagit.github.io/accessibility/reference/cost_to_closest.html) with the following, considering:

- The travel matrixes previously computed for each mode for each grid cell combination;
- The number of opportunities in each grid cell (grouped by grid cell, summed).

Calculations were performed to reach one, two and three opportunities.

The value computed represents the travel cost, in minutes, for that number of opportunities.

#### Results

##### Data tables

`freguesia_mobility_costs = st_read(IMPT_URL("/mobility_costs/r8/mobility_freguesia.gpkg"))`  
`municipio_mobility_costs = st_read(IMPT_URL("/mobility_costs/r8/mobility_municipio.gpkg"))`  
`grid_mobility_costs = st_read(IMPT_URL("/mobility_costs/r8/mobility_grid.gpkg"))`

##### Data structure (columns)

| Column                                                                        | Description                                                                                         | Example                                        |
| :---------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------------------- | :--------------------------------------------- |
| `Id dtmnfr municipio`                                                         | Geographical unit ID for grid Geographical unit ID for parish Geographical unit ID for municipality | 1 110501 Alcochete                             |
| `buildings                                     families kids elder residents` | Census data                                                                                         |                                                |
| `n_<POI>`                                                                     | Number of POIs for that category                                                                    | n_health                                       |
| `mobility_cost_<POI>_<mode>_n<1,2,3>_<census>`                                | Access to closest n POI category using mode for a category of census population                     | mobility_cost_health_primary_walk_n3_residents |

---

# 🛴 Mobility

# Mobility

By [miguel.alvelos@tecnico.ulisboa.pt](mailto:miguel.alvelos@tecnico.ulisboa.pt)

Code at [code/mobility.R](https://github.com/U-Shift/IMPT-data/blob/main/code/mobility.R)

Results at IMPT_URL(“/mobility”)

# Public Transport

## Availability/Coverage of Public Transport

    To compute coverage of public transport in the AML, we analysed every PT stop in the AML, taken from all the GTFS of the different operators in the region. From all of these stops, a buffer with a 500m radius was established.

    We then extracted the BGRI polygons’ centroids, and overlapped the stop buffers with these points, counting how many of them were inside the buffers. From here, we got the number of people within a 500-meter radius of a public transport stop. Disaggregating these results by freguesia, and dividing by the total number of residents in each freguesia, we got the ratio of population covered by public transportation.

**Limitations:**

- Stops are not disaggregated by mode nor by number of services or frequency. This is only a measure of how many people have _at least_ the most basic access to public transport.
- BGRI centroids can sometimes not be representative of the entire BGRI polygon (i.e. the stop buffer can reach the “outskirts” of the polygon but not the centroid, thus not counting the population even though some of it is served).

Results at IMPT_URL(“/mobility/freguesias_stops_coverage.csv”)

| Column                    | Description                                             |
| :------------------------ | :------------------------------------------------------ |
| `freguesia`               | Freguesia name                                          |
| `total_points`            | Total number of public transport stops in the freguesia |
| `served_population`       | Population within 500 meters of a public transport stop |
| `total_population`        | Total population of the freguesia                       |
| `ratio_served_population` | `served_population/total_population`                    |

## Shared Mobility Availability

    The input data was the location of all shared mobility docks (both physical and virtual), in the data provided by TML. Given this data, we counted the number of shared mobility points, both in each freguesia, and in each grid cell (H3, R8, as the one used calculating accessibility measures).

Results at IMPT_URL(“/mobility/freguesias_shared_mobility.csv”)

# Walking

## Existence of Infrastructure

    Our objective with this variable was to compute the ratio between pedestrian and road infrastructure. For this, we began by extracting road data from OSM. The following table shows the tags we used to get road and pedestrian infrastructure:

| Type of Infrastructure | OSM Tags |                |
| :--------------------: | :------: | :------------: |
|          Road          | highway  |    motorway    |
|                        |          |     trunk      |
|                        |          |    primary     |
|                        |          |   secondary    |
|                        |          |    tertiary    |
|                        |          |  unclassified  |
|                        |          |  residential   |
|                        |          | motorway_link  |
|                        |          |   trunk_link   |
|                        |          |  primary_link  |
|                        |          | secondary_link |
|                        |          | tertiary_link  |
|                        |          | living_street  |
|       Pedestrian       | highway  |    footway     |
|                        |          |  residential   |
|                        |          |   pedestrian   |
|                        |          |     steps      |
|                        | footway  |    sidewalk    |
|                        |          |    crossing    |
|                        |          |      path      |
|                        |          |    platform    |
|                        |          |    corridor    |
|                        |          |     alley      |
|                        |          |     track      |
|                        | sidewalk |      both      |
|                        |          |      left      |
|                        |          |     right      |

    For the road infrastructure, the objective was to get every (tagged) road available for cars, while for pedestrian infrastructure we attempted to get every road “possible to walk on”. Then, the length of all infrastructure was measured by freguesia, and the final result was given dividing the length of pedestrian infrastructure by that of the road infrastructure.

# Cycling

## Existence of Infrastructure

    The procedure for cycling existence of infrastructure was analogous to the pedestrian, but the data for cycleways was not taken from OSM, but rather from the file IMPT\_URL(“/mobility/AML\_cycle\_class.gpkg”).

## Quality of Infrastructure

    For cycling quality of infrastructure, our objective was to calculate the ratio between segregated cycleways and the total cycleway length, by freguesia.

We started by using the script at [code/ci_exctract_classify.R](https://github.com/U-Shift/IMPT-data/blob/main/code/ci_extract_classify.R) to classify the different types of cycle infrastructure in AML, in four categories:

- Ciclovia segregada – _Segregated cycleway_.
- Ciclofaixa pintada na via – _Cycle lane painted on road_.
- Infraestrutura pedonal partilhada – _Shared pedestrian infrastructure_.
- Via partilhada com veículos motorizados (sinalizada) – _Shared road with motorized vehicles (sinalized)_.

These results are available at IMPT_URL(“/mobility/AML_cycle_class.gpkg”). From this file, we disaggregated the results by freguesia, and proceeded to calculate the length of segregated cycleways, then computing the aforementioned ratio.

### Results

The results for walking and cycling can be seen in IMPT_URL(“/mobility/freguesias_infrastructure_ratio.csv”), and the table below indicates the data structure of the file.

| Column                             | Description                                          |
| :--------------------------------- | :--------------------------------------------------- |
| `dtmnfr freguesia Municipio nuts2` | Parish ID Parish name Municipality name NUTS 2 name  |
| `area_ha`                          | Area of the given parish                             |
| `road_length`                      | Total (driving) road length of the given parish      |
| `pedpath_length`                   | Length of all pedestrian paths in the given parish   |
| `cycleway_length`                  | Length of cycling infrastructure in the given parish |
| `segregated_cycleway_length`       | Length of segregated cycleways in the given parish   |
| `pedpath_to_road_ratio`            | `pedpath_length/road_length`                         |
| `cycleway_to_road_ratio`           | `cycleway_length/road_length`                        |
| `cycling_quality_ratio`            | `segregated_cycleway_length/cycleway_length`         |

# Commuting

By [Gonçalo André Ferreira Matos](mailto:goncaloafmatos@tecnico.ulisboa.pt)

## Travel time (peak)

Code at [code/mobility_commuting.R](https://github.com/U-Shift/IMPT-data/blob/main/code/mobility_commuting.R)

Flow chart at [code/flow_charts/mobility_commuting_travel_time_peak.png](http://github.com/U-Shift/IMPT-data/blob/main/code/flow_charts/mobility_commuting_travel_time_peak.png)

Considering the travel time matrixes computed for the hex grid for all modes and the jittering of the IMOB travel survey for commuting, we have computed the travel time for each geographical unit, considering the following algorithm:

1. Associate each jittered OD to a cell grid

2. For each mode and geographical unit, aggregate data, based on the trip origin, summarizing:
   1. Average travel time, weighted by number of trips
      1. When there is no travel option for an OD, we consider the maximum travel time for that mode on the other ODs (120 minutes)

   2. Total number of trips

   3. Total number of trips not accomplishable (Because there is no entry in the travel time matrix for the OD pair. This can happen because of the 120 minutes total travel time cut off, or transfers or walking time limit in the case of Public Transit. Refer to Travel Time Matrix, on Accessibility chapter for more details)

#### Results

##### Data tables

`freguesia_commuting = st_read(IMPT_URL("/mobility_commuting/freguesia_commuting.gpkg"))`  
`municipio_commuting = st_read(IMPT_URL("/mobility_commuting/municipio_commuting.gpkg"))`  
`grid_commuting = st_read(IMPT_URL("/mobility_commuting/grid_commuting.gpkg"))`

##### Data structure (columns)

| Column                | Description                                                                                                         | Example            |
| :-------------------- | :------------------------------------------------------------------------------------------------------------------ | :----------------- |
| `Id dtmnfr municipio` | Geographical unit ID for grid Geographical unit ID for parish Geographical unit ID for municipality                 | 1 110501 Alcochete |
| `avg_tt_<mode>`       | Average travel time for all commuting trips with origin in that polygon for that mode (weighted by number of trips) | avg_tt_car         |
| `trips`               | Total number of trips with origin in that polygon                                                                   | trips              |
| `trips_na_<mode>`     | Absolute number of trips with origin in that polygon for that mode that do not reach destination                    | trips_na_walk      |
| `PNA_<mode>`          | % of trips in that mode that do not reach destination                                                               | PNA_bike           |

## Number of transfers for key destinations (transit)

Code at [code/mobility_commuting.R\#L181](https://github.com/U-Shift/IMPT-data/blob/main/code/mobility_commuting.R#L181)

Flow chart at [code/flow_charts/mobility_commuting_transfers.png](http://github.com/U-Shift/IMPT-data/blob/main/code/flow_charts/mobility_commuting_transfers.png)

To compute the number of transfers for key destinations, we considered the Travel Time Matrixes for Public Transit (Refer to Travel Time Matrix, on Accessibility chapter for more details) computed for 0 (direct trip), 1, 2, 3 and 4 transfer cut offs.

Given this data and for each jittered OD pair associated to the corresponding origin and destination cell grid, we determined the number of transfers needed, by checking if each of the matrices for the growing number of transfers would have a match for that OD pair.

Based on this procedure, we determined the number of transfers for 2649 out of the 18997 jittered ODs (86.1% of total OD pairs and 87.65% of total trips) and aggregated them for the several geographical units, summarizing:

- Number of jittered ODs
- Number of jittered ODs with trips not accomplishable in PT (Because there is no entry in the travel time matrix for the OD pair. This can happen because of the 120 minutes total travel time cut off, or transfers or walking time limit in the case of Public Transit. Refer to Travel Time Matrix, on Accessibility chapter for more details)
- Average number of transfers, weighted by the number of trips
- Total number of trips
- Total number of transfers

#### Results

##### Data tables

`freguesia_transfers = st_read(IMPT_URL("/mobility_commuting/freguesia_transfers.gpkg"))`  
`municipio_transfers = st_read(IMPT_URL("/mobility_commuting/municipio_transfers.gpkg"))`  
`grid_transfers = st_read(IMPT_URL("/mobility_commuting/grid_transfers.gpkg"))`

##### Data structure (columns)

| Column                    | Description                                                                                         | Example            |
| :------------------------ | :-------------------------------------------------------------------------------------------------- | :----------------- |
| `Id dtmnfr municipio`     | Geographical unit ID for grid Geographical unit ID for parish Geographical unit ID for municipality | 1 110501 Alcochete |
| `nr_jitters`              | Number of jittered ODs                                                                              |                    |
| `nr_jitters_na`           | Number of jittered ODs with trips not accomplishable in PT                                          |                    |
| `weighted_mean_transfers` | Average number of transfers, weighted by the number of trips                                        |                    |
| `trips`                   | Total number of trips                                                                               |                    |
| `total_transfers`         | Total number of transfers                                                                           |                    |

# Public transit

By [Gonçalo André Ferreira Matos](mailto:goncaloafmatos@tecnico.ulisboa.pt)

## PT Waiting Times \+ night/weekend service availability

Code at [code/mobility_transit.R](https://github.com/U-Shift/IMPT-data/blob/main/code/mobility_transit.R)

Flow chart at [code/flow_charts/mobility_transit.png](https://github.com/U-Shift/IMPT-data/blob/main/code/flow_charts/mobility_transit.png)

To compute frequencies, headways and waiting times for transit, we considered:

1. Frequencies at stops computed with [tidytransit::get_stop_frequency](https://r-transit.github.io/tidytransit/reference/get_stop_frequency.html), that returned the number of services and the headway (departures divided by timespan) in seconds as columns, computed both for:
   1. Wednesday, 04/02/2026
      1. Full day
      2. Peak hour (08:00 to 09:00)
      3. Night (22:00 to 23:00)
   2. Sunday, 08/02/2026
      1. From 10:00 to 11:00 (service peak for Carris Metropolitana)
2. A buffer of 500 meters around each stop (for any mode) to account for number of people served (assuming overlap between buffer and BGRI centroids)

For each mode and geographical unit, data was aggregated, summarizing:

- Headway (for each period), in seconds, weighted by population served
- Waiting time (for each period), in minutes, weighted by population served (inverse of the previous)
- Frequency (for each period), weighted by population served
- Total population served
- Total frequency (for each period)
  - Mind that a frequency is a bus passing at a stop. A route with 20 stops will have 20 frequencies.
- Total number of stops
- Frequency reduction at night and weekends (total number of services)
- Weighted frequency reduction at night and weekends (considering frequencies weighted by population)

#### Results

##### Data tables

`freguesia_transit = st_read(IMPT_URL("/mobility_transit/freguesias_headways.gpkg"))`  
`municipio_transit = st_read(IMPT_URL("/mobility_transit/municipios_headways.gpkg"))`  
`grid_transit = st_read(IMPT_URL("/mobility_transit/grid_headways.gpkg"))`

##### Data structure (columns)

| Column                                           | Description                                                                                         | Example            |
| :----------------------------------------------- | :-------------------------------------------------------------------------------------------------- | :----------------- |
| `Id dtmnfr municipio`                            | Geographical unit ID for grid Geographical unit ID for parish Geographical unit ID for municipality | 1 110501 Alcochete |
| `total_population_served`                        | Total population served                                                                             |                    |
| `weighted_headway_<peak/day/night/weekend>`      | Headway (peak and day), in seconds, weighted by population served                                   |                    |
| `weighted_waiting_time_<peak/day/night/weekend>` | Waiting time (peak and day), in minutes, weighted by population served (inverse of the previous)    |                    |
| `weighted_frequency_<peak/day/night/weekend>`    | Frequency (peak and day), weighted by population served                                             |                    |
| `total_frequency_<peak/day/night/weekend>`       | Total frequency (peak and day)                                                                      |                    |
| `frequency_reduction_<night/weekend>`            | Frequency reduction at night and weekends (total number of services)                                |                    |
| `weighted_frequency_reduction_<night/weekend>`   | Weighted frequency reduction at night and weekends (considering frequencies weighted by population) |                    |
| `n_stops`                                        | Total number of stops                                                                               |                    |

---

# 💶 Affordability

# Affordability

TODO\!

# Commuting Affordability

To compute commuting costs for car and PT, we used [r5r::detailed_itineraries](https://ipeagit.github.io/r5r/reference/detailed_itineraries.html) method to compute, for each commuting itinerary (using IMOB 2017 Jittering, refer to Accessibility section for more details), the detailed journey and then calculate its costs. The calculation methodology was different for each mode.

Mind that we used the full commuting database from IMOB, regardless of the mode people reported to use. The idea was to estimate the costs if everyone that commutes in the Lisbon Metropolitan Area used the car or PT.

### For car

Code at [https://github.com/U-Shift/IMPT-data/blob/main/code/mobility_costs_money_car.R](https://github.com/U-Shift/IMPT-data/blob/main/code/mobility_costs_money_car.R)

The total cost for a car trip was determined, considering:

- The distance, multiplied by a fixed cost per km, according to [Portaria n.º 1553-D/2008, de 31 de dezembro](https://diariodarepublica.pt/dr/detalhe/portaria/1553-d-2008-243733), which sets the car expenses allowance at 0,40€/km, already accounting for fuel and car maintenance costs
- The tools, computed using the [official Infrastruturas de Portugal website](https://portagens.infraestruturasdeportugal.pt/) to compute tool costs for Portugal, through its API (using the trajectory geometry)

### For Public Transport

Code at [https://github.com/U-Shift/IMPT-data/blob/main/code/mobility_costs_money_pt.R](https://github.com/U-Shift/IMPT-data/blob/main/code/mobility_costs_money_pt.R) and [https://github.com/U-Shift/IMPT-data/blob/main/code/mobility_costs_money_pt_cost_structure.r](https://github.com/U-Shift/IMPT-data/blob/main/code/mobility_costs_money_pt_cost_structure.r)

The fare for a public transport itinerary was computed by r5r, considering a fare structure set by us. We have computed the costs for two types of fares.

**Scenario A: Single tickets (Prepaid/On-board)**  
 Calculation based on Carris Lisbon Data:

- Total single ticket validations \= 17,424,000
- Total single ticket revenue \= 33,278(k) € \= 33,278,000 €
- Average cost per single ticket \= 33,278,000 / 17,424,000 \= \~1.91 €  
   (While Zapping is 1.72-1.90, blending the 10.7% on-board revenue at 2.30€ puts the overall average at 1.91€)

This fare structure assumes free transfers in the same mode for 60 minutes. Transfers between different modes are charged a new ticket.

Transfers between the same mode does not imply that they are from the same agency. For instance Carris to Carris metropolitana (Bus \- Bus) is charging a single ticket, instead of the actual 2\. This can be adapted with rules by route, but for now we opted to simplify the fare costs.

**Scenario B: Monthly Pass**  
 Calculation based on operator data:

1. Total Pass Revenue: (5063510 \* 40€) \+ (887190 \* 30€) \+ (1103782 \* 20€) \+ (243784 \* 80€) \+ (3425013 \* 0€) \= 270,734,460 €
2. Total Pass Recharges (months): 10,723,279
3. Average Paid per Pass Month: 270,734,460 € / 10,723,279 \= \~25.25 €
4. Approximating 44 journeys/month (2 trips a day \* 22 working days), the cost per journey \= 25.25 € / 44 \= \~0.57 €  
   (Note: the 3.4M free young passes significantly decrease the base average cost paid per user).

### Results

#### Data tables

`freguesia_affordability_car = st_read(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_car.gpkg"))`  
`municipio_affordability_car = st_read(IMPT_URL("/mobility_money_costs/municipio_commuting_money_car.gpkg"))`  
`grid_affordability_car = st_read(IMPT_URL("/mobility_money_costs/grid_commuting_money_car.gpkg"))`  
`freguesia_affordability_pt_single_fare = st_read(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_pt_single_fare.gpkg"))`  
`municipio_affordability_pt_single_fare = st_read(IMPT_URL("/mobility_money_costs/municipio_commuting_money_pt_single_fare.gpkg"))`  
`grid_affordability_pt_single_fare = st_read(IMPT_URL("/mobility_money_costs/grid_commuting_money_pt_single_fare.gpkg"))`  
`freguesia_affordability_pt_pass = st_read(IMPT_URL("/mobility_money_costs/freguesia_commuting_money_pt_pass_fare.gpkg"))`  
`municipio_affordability_pt_pass = st_read(IMPT_URL("/mobility_money_costs/municipio_commuting_money_pt_pass_fare.gpkg"))`  
`grid_affordability_pt_pass = st_read(IMPT_URL("/mobility_money_costs/grid_commuting_money_pt_pass_fare.gpkg"))`

#### Data structure (columns)

| Column                | Description                                                                                         | Example            |
| :-------------------- | :-------------------------------------------------------------------------------------------------- | :----------------- |
| `Id dtmnfr municipio` | Geographical unit ID for grid Geographical unit ID for parish Geographical unit ID for municipality | 1 110501 Alcochete |
| `total_money`         | Monetary cost of commuting trips (€) for region, weighted by number of trips                        | \-                 |
| `avg_tt`              | Average travel time of commuting trips (min), weighted by number of trips                           | \-                 |
| `avg_distance`        | Average distance of commuting trips (meters), weighted by number of trips                           | \-                 |
| `trips`               | Total number of trips with origin on that region                                                    | \-                 |

---

# 🛡️ Safety

# Safety

By [margaridapimentelcarvalho@tecnico.ulisboa.pt](mailto:margaridapimentelcarvalho@tecnico.ulisboa.pt)

TODO\!

Accidents computed by [rosamfelix@tecnico.ulisboa.pt](mailto:rosamfelix@tecnico.ulisboa.pt) ignore those outside the administrative boundaries of the municipalities, such as those on the 25 de Abril and Vasco da Gama bridges.

# Accessibility drafts

# Accessibility

## Geographical unit

We [have decided](https://github.com/U-Shift/IMPT-data/issues/5) to compute accessibility measures using the H3 grid with medium resolution (8, edge length of 559m).  
![][image1]

N \= 3686

Min. 1st Qu. Median Mean 3rd Qu. Max.  
 1.00 6.00 12.50 29.69 34.00 350.00

Total_points mean_points median_points min_points max_points sd_points  
 3681 29.68548 12.5 1 350 50.89689

![][image2]  
![][image3]

**(With res 9\)**  
N= 25890

Min. 1st Qu. Median Mean 3rd Qu. Max.  
 10.0 37.0 93.5 208.5 234.2 2446.0  
Total_points mean_points median_points min_points max_points sd_points  
 25853 208.4919 93.5 10 2446 358.4453

![][image4]  
![][image5]

## Calculations

[`r5r::travel_time_matrix()`](https://ipeagit.github.io/r5r/reference/travel_time_matrix.html)  
The calculations will be performed using [r5r](https://ipeagit.github.io/r5r/index.html) and [accessibility](https://ipeagit.github.io/accessibility/index.html) R packages, used to compute a **travel time matrix** for every hex cell, for each mode, using a max travel time of 1 and 2 hours, with some variations for transit: peak, non-peak and night AND 1 and 2 max transfers.

The median value (p50) will be used for the travel time in each mode.

Given the travel times between all grid cells, we will then determine:

### Accessibility to essential services

[`accessibility::cumulative_cutoff(active=TRUE)`](https://ipeagit.github.io/accessibility/reference/cumulative_cutoff.html)

Number of opportunities accessible in X time (cut off)  
Based on [https://u-shift.github.io/Traffic-Simulation-Models/accessibility.html\#accessibility](https://u-shift.github.io/Traffic-Simulation-Models/accessibility.html#accessibility)

### Mobility to essential services

[`accessibility::cumulative_cutoff(active=TRUE)`](https://ipeagit.github.io/accessibility/reference/cumulative_cutoff.html)  
Number of people that can reach opportunities, make sure to add population data  
We need to adapt this to consider travel time

[`accessibility::cost_to_closest()`](https://ipeagit.github.io/accessibility/reference/cost_to_closest.html) `// accessibility::floating_catchment_area()`  
Travel cost (time) to closest opportunity (n=1, n=3)

### Jobs accessibility

We need INE data on the number of jobs per county.

### Jobs mobility

Jittering IMOB 2017\. Jittering at district level? based on trip purpose ("ir para o trabalho".)

## How will we aggregate them by district/municipality?

We could aggregate hex grids by county/municipality, considering the median.
