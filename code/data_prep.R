# Data preparation, mainly geometries

library(sf)
library(dplyr)
library(mapview)


# Geo polygons ------------------------------------------------------------


# dowload and extract with sf
# link_DGT = "https://geo2.dgterritorio.gov.pt/caop/CAOP_Continente_2024_1-gpkg.zip"
# temp <- "/data/IMPT/original/caop2024.zip"
# download.file(link_DGT, temp, mode = "wb")
# unzip(temp, exdir = "/data/IMPT/original/")
CAOP_PT = st_read("/data/IMPT/original/Continente_CAOP2024_1.gpkg")

# selecionar apenas NUT II Lisboa
CAOP_GLPS = CAOP_PT |> 
  filter(nuts2 %in% c("Grande Lisboa", "Península de Setúbal")) |>
  select(-id, -nuts1, -nuts3, - tipo_area_administrativa, -distrito_ilha, -perimetro_km) |>
  st_transform(crs = 4326)
names(CAOP_GLPS)

# recortar freguesias de Lisboa para remover rio 
freguesias_lx_recortadas = st_read("/data/IMPT/mqat/FREGUESIASgeo.gpkg") |> 
  left_join(readRDS("/data/IMPT/mqat/Metadata_Freguesias.Rds") |> select(Dicofre, Freguesia), by="Dicofre") |>
  filter(Concelho == "Lisboa") |>
  select(Dicofre, geom) |> 
  rename(dtmnfr=Dicofre) |> 
  left_join(CAOP_GLPS |> sf::st_drop_geometry(), by="dtmnfr")

CAOP_GLPS = CAOP_GLPS |> 
  filter(municipio != "Lisboa") |> 
  rbind(freguesias_lx_recortadas)

# For freguesias with multiple polygons, choose the one with greatest area_ha, without loosing other attributes
CAOP_GLPS_UNIQUE_dtmnfr = CAOP_GLPS |> 
  group_by(dtmnfr) |>
  slice_max(order_by = area_ha, n=1, with_ties = FALSE) |>
  ungroup()

freguesias = CAOP_GLPS_UNIQUE_dtmnfr

# mapview(CAOP_GLPS)
# View(CAOP_GLPS)

st_write(CAOP_GLPS, "/data/IMPT/geo/freguesias_2024.gpkg", delete_dsn = TRUE)
st_write(CAOP_GLPS_UNIQUE_dtmnfr, "/data/IMPT/geo/freguesias_2024_unique.gpkg", delete_dsn = TRUE)
freguesias = st_read("/data/IMPT/geo/freguesias_2024_unique.gpkg")

# group sf by municipio
municipios = CAOP_GLPS |> 
  group_by(municipio) |>
  summarise(geometry = st_union(geom)) |>
  st_transform(crs = 4326)
municipios = municipios [-6, ] # Remove strange Lisbon

# mapview(municipios)
st_write(municipios, "/data/IMPT/geo/municipios_2024.gpkg", delete_dsn = TRUE)
municipios = st_read("/data/IMPT/geo/municipios_2024.gpkg")

# for the whole limit (to HOT export tool)
municipios_union = municipios |> sf::st_union() |> sf::st_make_valid()
st_write(municipios_union, "/data/IMPT/geo/municipios_union_2024.geojson", delete_dsn = TRUE)
municipios_union = st_read("/data/IMPT/geo/municipios_union_2024.geojson")

# for the bbox (to Copernicus)
municipios_union_bbox = st_as_sfc(st_bbox(municipios_union))
st_write(municipios_union_bbox, st, delete_dsn = TRUE)

# OSM data ----------------------------------------------------------------

# Road network exported using Hot Exports Tool, https://export.hotosm.org/exports/4782f0b8-6778-4c0e-8e4f-97fc62e7f240, to generate .pbf file for r5r
road_network = st_read("/data/IMPT/geo/IMPT_Road_network.gpkg")

# # filter main roads
# 1-4
road_network_main = road_network |>
  filter(highway %in% c("primary", "secondary", "tertiary", "trunk", "motorway")) |>
  select(osm_id, name, highway)
mapview::mapview(road_network_main, zcol = "highway")

# filter even more roads
# 1-3
road_network_base = road_network_main |>
  filter(!highway %in% "tertiary")
mapview::mapview(road_network_base, zcol = "highway")

st_write(road_network_main, "/data/IMPT/geo/road_network_main.gpkg", delete_dsn = TRUE)
st_write(road_network_base, "/data/IMPT/geo/road_network_base.gpkg", delete_dsn = TRUE)

# Trips -------------------------------------------------------------------

# trips_freguesias_2011 = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/TRIPSmode_freguesias.Rds"))
trips_freguesias_2016 = readRDS(url("https://github.com/U-Shift/MQAT/raw/refs/heads/main/data/TRIPSmode_freg.Rds"))

FREGUESIASgeo_2016 = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/FREGUESIASgeo.Rds"))
trips_freguesias_2016_sf = trips_freguesias_2016 |> 
  select(Origin_dicofre16) |>
  distinct() |>
  left_join(FREGUESIASgeo_2016 |> select(Dicofre), by=c("Origin_dicofre16"="Dicofre")) |>
  st_as_sf(crs=4326)


## conversion --------------------------------------------------------------

# Identify freguesias that were created or removed between 2016 and 2024
difference_created = setdiff(freguesias$dtmnfr, trips_freguesias_2016_sf$Origin_dicofre16)
difference_removed = setdiff(trips_freguesias_2016_sf$Origin_dicofre16, freguesias$dtmnfr)

st_write(freguesias |> filter(dtmnfr %in% difference_created), "/data/IMPT/geo/freguesias_created_2024.geojson", delete_dsn = TRUE)
st_write(trips_freguesias_2016_sf |> filter(Origin_dicofre16 %in% difference_removed) |> unique(), "/data/IMPT/geo/freguesias_removed_2024.geojson", delete_dsn = TRUE)


# After QGIS inspection, manual conversion table created
conversion_dicofre = data.frame(old=character(), new=character())
conversion_dicofre = conversion_dicofre |> bind_rows(data.frame(old="151007", new="151008"))
conversion_dicofre = conversion_dicofre |> bind_rows(data.frame(old="151007", new="151009"))
conversion_dicofre = conversion_dicofre |> bind_rows(data.frame(old="151007", new="151010"))
conversion_dicofre = conversion_dicofre |> bind_rows(data.frame(old="111127", new="111134"))
conversion_dicofre = conversion_dicofre |> bind_rows(data.frame(old="111127", new="111135"))
conversion_dicofre = conversion_dicofre |> bind_rows(data.frame(old="111123", new="111129"))
conversion_dicofre = conversion_dicofre |> bind_rows(data.frame(old="111123", new="111131"))
conversion_dicofre = conversion_dicofre |> bind_rows(data.frame(old="111123", new="111132"))
conversion_dicofre = conversion_dicofre |> bind_rows(data.frame(old="111126", new="111130"))
conversion_dicofre = conversion_dicofre |> bind_rows(data.frame(old="111126", new="111133"))

write.csv(conversion_dicofre, "useful_data/dicofre_16_24_conversion.csv", row.names = FALSE)
conversion_dicofre = read.csv("useful_data/dicofre_16_24_conversion.csv")

## create a useful conversion dicofre with all, even the ones that did not change
freguesias16 = trips_freguesias_2016 |> 
  
  select(Origin_dicofre16) |>
  distinct() |>
  rename(dtmnfr16 = Origin_dicofre16)

all_dicofre_conversion = freguesias16 |> 
  filter(!dtmnfr16 %in% conversion_dicofre$old) |> 
  mutate(dtmnfr24 = dtmnfr16) |>
  bind_rows(conversion_dicofre |> 
              mutate(old = as.character(old),
                     new = as.character(new)) |>
              rename(dtmnfr16 = old, dtmnfr24 = new))

# provide a weight based on the area changed, for the ones the same, weight = 1

all_dicofre_conversion_weight = all_dicofre_conversion |>
  left_join(
    freguesias |> st_drop_geometry() |> select(dtmnfr, area_ha) |> rename(dtmnfr16 = dtmnfr, area_ha_16 = area_ha),
    by=c("dtmnfr16")
  ) |>
  left_join(
    freguesias |> st_drop_geometry() |> select(dtmnfr, area_ha) |> rename(dtmnfr24 = dtmnfr, area_ha_24 = area_ha),
    by=c("dtmnfr24")
  ) |>
  mutate(
    area_change = abs(area_ha_24 - area_ha_16),
    weight = ifelse(area_change == 0, 1, area_ha_24 / (area_ha_16 + area_change))
  )

all_dicofre_conversion_weight_subset = all_dicofre_conversion_weight |> 
  filter(is.na(area_change)) |> 
  group_by(dtmnfr16) |>
  summarise(area_ha_16 = sum(area_ha_24))
all_dicofre_conversion_weight_subset = all_dicofre_conversion_weight |> 
  filter(is.na(area_change)) |> 
  select(dtmnfr16, dtmnfr24, area_ha_24) |>
  left_join(all_dicofre_conversion_weight_subset)
all_dicofre_conversion_weight_subset = all_dicofre_conversion_weight_subset |>
  mutate(weight = area_ha_24 / area_ha_16 ) 

all_dicofre_conversion_weight = all_dicofre_conversion_weight |> 
  filter(!is.na(area_change)) |> 
  bind_rows(all_dicofre_conversion_weight_subset) |> 
  select(-area_change)

rm(all_dicofre_conversion_weight_subset)

# save
write.csv(all_dicofre_conversion, "useful_data/dicofre_16_24_conversion_full.csv", row.names = FALSE)
saveRDS(all_dicofre_conversion, "useful_data/dicofre_16_24_conversion_full.Rds")
write.csv(all_dicofre_conversion_weight, "useful_data/dicofre_16_24_conversion_full_with_weights.csv", row.names = FALSE)
saveRDS(all_dicofre_conversion_weight, "useful_data/dicofre_16_24_conversion_full_with_weights.Rds")
# load
all_dicofre_conversion = readRDS("useful_data/dicofre_16_24_conversion_full.Rds")
all_dicofre_conversion_weight = readRDS("useful_data/dicofre_16_24_conversion_full_with_weights.Rds")

# Adjust trips to new dicofre ids
trips_freguesias_to_convert = trips_freguesias_2016 |> filter(
  Origin_dicofre16 %in% conversion_dicofre$old |
  Destination_dicofre16 %in% conversion_dicofre$old
)
nrow(trips_freguesias_to_convert) # 535

# Some freguesias have multiple entries (different polygons)
# mapview(freguesias |> filter(dtmnfr=="110501"), zcol="area_ha")
# mapview(freguesias |> filter(dtmnfr=="110508"), zcol="area_ha")
freguesias_unique_dtmnfr = freguesias |> 
  st_drop_geometry() |>
  group_by(dtmnfr) |>
  summarise(area_ha = sum(area_ha)) |> 
  ungroup()

# Add columns with converted dicofre ids and areas for weighted distribution
trips_freguesias_conversion = trips_freguesias_to_convert |>
  # Convert DICOFRE
  left_join(conversion_dicofre |> rename(Origin_dicofre24 = new), by =c("Origin_dicofre16"="old"), relationship = "many-to-many")  |>
  left_join(conversion_dicofre |> rename(Destination_dicofre24 = new), by =c("Destination_dicofre16"="old"), relationship = "many-to-many") |>
  # Get areas for weighted distribution
  left_join(freguesias_unique_dtmnfr |> rename(Origin_area_ha = area_ha), by=c("Origin_dicofre24"="dtmnfr")) |>
  left_join(freguesias_unique_dtmnfr |> rename(Destination_area_ha = area_ha), by=c("Destination_dicofre24"="dtmnfr")) 
nrow(trips_freguesias_conversion) # 1375

nrow(trips_freguesias_conversion |> filter(is.na(Origin_dicofre24) | is.na(Destination_dicofre24)))
# View(trips_freguesias_conversion |> filter(is.na(Origin_dicofre24) | is.na(Destination_dicofre24)))
nrow(trips_freguesias_conversion |> filter(!is.na(Origin_dicofre24) & !is.na(Destination_dicofre24)))
# View(trips_freguesias_conversion |> filter(!is.na(Origin_dicofre24) & !is.na(Destination_dicofre24)))

# Distribute trips proportionally to area
# Different calculation depending on whether origin, destination or both were changed
trips_freguesias_conversion_origin_disaggregated = trips_freguesias_conversion |> 
  filter(!is.na(Origin_dicofre24) & is.na(Destination_dicofre24)) |>
  group_by(Origin_dicofre16, Destination_dicofre16) |>
  mutate(
    weight = Origin_area_ha / sum(Origin_area_ha)
  ) |>
  ungroup()
# View(trips_freguesias_conversion_origin_disaggregated)

trips_freguesias_conversion_destination_disaggregated = trips_freguesias_conversion |> 
  filter(is.na(Origin_dicofre24) & !is.na(Destination_dicofre24)) |>
  group_by(Origin_dicofre16, Destination_dicofre16) |>
  mutate(
    weight = Destination_area_ha / sum(Destination_area_ha)
  ) |>
  ungroup()
# View(trips_freguesias_conversion_destination_disaggregated)

trips_freguesias_conversion_both_disaggregated = trips_freguesias_conversion |> 
  filter(!is.na(Origin_dicofre24) & !is.na(Destination_dicofre24)) |>
  group_by(Origin_dicofre16, Destination_dicofre16) |>
  mutate(
    weight = (Origin_area_ha+Destination_area_ha) / (sum(Destination_area_ha)+sum(Origin_area_ha))
  ) |>
  ungroup()
# View(trips_freguesias_conversion_both_disaggregated)

trips_freguesias_converted = trips_freguesias_conversion_origin_disaggregated |>
  bind_rows(trips_freguesias_conversion_destination_disaggregated) |>
  bind_rows(trips_freguesias_conversion_both_disaggregated) 
nrow(trips_freguesias_converted)

# Validate results
assertthat::assert_that(nrow(trips_freguesias_converted) == nrow(trips_freguesias_conversion))
nrow(trips_freguesias_converted |> select(Origin_dicofre16, Destination_dicofre16) |> distinct()) # 535
table( (trips_freguesias_converted |> 
  group_by(Origin_dicofre16, Destination_dicofre16) |>
  summarise(total_weight = sum(weight)))$total_weight ) # Must return 1 x 535

# Recalculate trip counts
trips_freguesias_adjusted = trips_freguesias_converted |>
  mutate(
    Total = Total*weight,
    Walk = Walk*weight,
    Bike = Bike*weight,
    Car = Car*weight,
    PTransit = PTransit*weight,
    Other = Other*weight,
    Origin_dicofre24 = ifelse(is.na(Origin_dicofre24), Origin_dicofre16, Origin_dicofre24),
    Destination_dicofre24 = ifelse(is.na(Destination_dicofre24), Destination_dicofre16, Destination_dicofre24)
  ) 

# Validate total values remain the same
assertthat::assert_that(sum(trips_freguesias_adjusted$Total) == sum(trips_freguesias_to_convert$Total))
assertthat::assert_that(sum(trips_freguesias_adjusted$Walk) == sum(trips_freguesias_to_convert$Walk))
assertthat::assert_that(sum(trips_freguesias_adjusted$Bike) == sum(trips_freguesias_to_convert$Bike))
assertthat::assert_that(sum(trips_freguesias_adjusted$Car) == sum(trips_freguesias_to_convert$Car))
assertthat::assert_that(sum(trips_freguesias_adjusted$PTransit) == sum(trips_freguesias_to_convert$PTransit))
assertthat::assert_that(sum(trips_freguesias_adjusted$Other) == sum(trips_freguesias_to_convert$Other))

# Finally, replace original data
trips_freguesias_2024 = trips_freguesias_2016 |> 
  # Remove those that changed DICOFRE
  filter(
    !Origin_dicofre16 %in% conversion_dicofre$old
    & !Destination_dicofre16 %in% conversion_dicofre$old
  ) |> 
  # The ones that remain, did not change, so just rename DICOFRE columns
  rename(
    Origin_dicofre24 = Origin_dicofre16,
    Destination_dicofre24 = Destination_dicofre16
  ) |>
  # Add the adjusted ones
  bind_rows(
    trips_freguesias_adjusted |> 
      select(
        Origin_dicofre24,
        Destination_dicofre24,
        Total,
        Walk,
        Bike,
        Car,
        PTransit,
        Other
      )
  )

# Validate final results
assertthat::assert_that(
  # Final rows
  nrow(trips_freguesias_2024) == (
    # Remove those that changed DICOFRE
    nrow(trips_freguesias_2016 |> 
      filter(
        !Origin_dicofre16 %in% conversion_dicofre$old
        & !Destination_dicofre16 %in% conversion_dicofre$old
    )) + nrow(trips_freguesias_conversion)))

assertthat::assert_that(sum(trips_freguesias_2016$Total) == sum(trips_freguesias_2024$Total))
assertthat::assert_that(sum(trips_freguesias_2016$Walk) == sum(trips_freguesias_2024$Walk))
assertthat::assert_that(sum(trips_freguesias_2016$Bike) == sum(trips_freguesias_2024$Bike))
assertthat::assert_that(sum(trips_freguesias_2016$Car) == sum(trips_freguesias_2024$Car))
assertthat::assert_that(sum(trips_freguesias_2016$PTransit) == sum(trips_freguesias_2024$PTransit))
assertthat::assert_that(sum(trips_freguesias_2016$Other) == sum(trips_freguesias_2024$Other))

saveRDS(trips_freguesias_2024, "/data/IMPT/trips/TRIPSmode_freguesias_2024.Rds")
trips_freguesias_2024 = readRDS("/data/IMPT/trips/TRIPSmode_freguesias_2024.Rds")


# Jittering ---------------------------------------------------------------
# Adaptted from https://u-shift.github.io/Traffic-Simulation-Models/jittering.html

# remotes::install_github("itsleeds/odjitter", subdir = "r")
library(odjitter)

# NÃO SEI SE É PRECISO UM setseed(42) para termos sempre o mesmo resultado...


# Jitter with disagregation threshold of 200 trips
od_freguesias_jittered = odjitter::jitter(  
  od = trips_freguesias_2024,
  zones = freguesias,
  subpoints = road_network, 
  disaggregation_key = "Total",
  disaggregation_threshold = 200
)

# Validate calcuations
assertthat::assert_that(sum(od_freguesias_jittered$Total) == sum(trips_freguesias_2024$Total))
assertthat::assert_that(sum(od_freguesias_jittered$Walk) == sum(trips_freguesias_2024$Walk))
assertthat::assert_that(sum(od_freguesias_jittered$Bike) == sum(trips_freguesias_2024$Bike))
assertthat::assert_that(sum(od_freguesias_jittered$Car) == sum(trips_freguesias_2024$Car))
assertthat::assert_that(sum(od_freguesias_jittered$PTransit) == sum(trips_freguesias_2024$PTransit))
assertthat::assert_that(sum(od_freguesias_jittered$Other) == sum(trips_freguesias_2024$Other))

mapview::mapview(od_freguesias_jittered, lwd = 0.2)

# add an id to the jittered pairs, so we can join later
od_freguesias_jittered_id = od_freguesias_jittered
od_freguesias_jittered_id$id = 1:nrow(od_freguesias_jittered_id)

st_write(od_freguesias_jittered_id, "/data/IMPT/trips/od_freguesias_jittered_2024.gpkg", delete_dsn = TRUE)
od_freguesias_jittered_id = st_read("/data/IMPT/trips/od_freguesias_jittered_2024.gpkg")


## Origins and Destinations as points

library(stplanr)

#with stplanr
od_freguesias_jittered_points = line2df(od_freguesias_jittered)
od_freguesias_jittered_OR = od_freguesias_jittered_points |>
  select(L1, fx, fy) |> # from
  rename(id = L1,
         lon = fx,
         lat = fy)
od_freguesias_jittered_DE = od_freguesias_jittered_points |>
  select(L1, tx, ty) |> # to
  rename(id = L1,
         lon = tx,
         lat = ty)

# as sf
od_freguesias_jittered_OR_geo = st_as_sf(od_freguesias_jittered_OR,
                                            coords = c("lon", "lat"),
                                            crs = 4326)
od_freguesias_jittered_DE_geo = st_as_sf(od_freguesias_jittered_DE,
                                            coords = c("lon", "lat"),
                                            crs = 4326)

# mapview(od_freguesias_jittered_OR_geo, col.regions = "red") + 
#   mapview(od_freguesias_jittered_DE_geo, col.regions = "blue")

st_write(od_freguesias_jittered_OR_geo, "/data/IMPT/trips/od_freguesias_jittered200_OR.gpkg", delete_dsn = TRUE)
st_write(od_freguesias_jittered_DE_geo, "/data/IMPT/trips/od_freguesias_jittered200_DE.gpkg", delete_dsn = TRUE)
od_freguesias_jittered_OR_geo = st_read("/data/IMPT/trips/od_freguesias_jittered200_OR.gpkg")
od_freguesias_jittered_DE_geo = st_read("/data/IMPT/trips/od_freguesias_jittered200_DE.gpkg")



# Census 21 data ----------------------------------------------------------

## download and extract zip for /data/IMPT folder
# ceunsus_url = "https://mapas.ine.pt/download/filesGPG/2021/nuts3/BGRI21_170.zip"
# temp <- "/data/IMPT/original/census2021.zip"
# download.file(ceunsus_url, temp, mode = "wb")
# unzip(temp, exdir = "/data/IMPT/original/")
Census21_BGRI = st_read("/data/IMPT/original/BGRI21_170.gpkg")

## make sure there is no polygon missing or exclude extra ones
# mapview::mapview(Census21_BGRI) + mapview(municipios_union, col.regions = "red")
# they are the same areas!

# from polygons to points
census_points21 = Census21_BGRI |> 
  st_centroid() |> 
  st_transform(4326) # make sue it is in universal CRS
plot(census_points21$geom) # census units in points
names(census_points21)

# create a new variable dicofre24 that match the freguesias spatially intersection the census points with the freguesias polygons
census_points24 = census_points21 |> 
  st_join(freguesias |> select(dtmnfr, freguesia, municipio), join = st_intersects, left=TRUE) |> 
  rename(dicofre24 = dtmnfr) |> 
  mutate(id = BGRI2021) |> 
  #move id var for the beginning
  select(id, everything())
 
# save
st_write(census_points24, "/data/IMPT/geo/census24_points.gpkg", delete_dsn = TRUE)
census_points24 = st_read("/data/IMPT/geo/census24_points.gpkg")



# POIs --------------------------------------------------------------------

pois_pt = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/osm_poi_landuse.gpkg")
pois = pois_pt[municipios_union,] # spatial filter
table(pois$group)
# amenity healthcare    leisure       shop      sport    tourism 
# 13761        757       3150      11245       2984       1620 

# mapview::mapview(pois, zcol = "group")

# save and load
st_write(pois, "/data/IMPT/geo/pois_osm2024.gpkg", delete_dsn = TRUE)
pois = st_read("/data/IMPT/geo/pois_osm2024.gpkg")

pois_health = read.csv("https://github.com/carrismetropolitana/datasets/raw/refs/heads/latest/facilities/health_centers/health_centers.csv") |>
  filter(
    # https://www.cm-seixal.pt/noticia/autarquia-participa-em-audicao-sobre-hospital-no-seixal
    name!="futuro Hospital do Seixal"
  ) |>
  mutate(
    lon = ifelse(id=="HC0077", -1*lon, lon)
  ) |>
  select(id, name, lat, lon) |>
  mutate(
    type = case_when(
      grepl("Hospital", name) ~ "Hospital",
      grepl("Centro de Medicina", name) ~ "Hospital",
      # grepl("Familiar", name) ~ "USF", # To validate if Centro de Saúde and USF overlap (occurs in some cases)
      TRUE ~ "Centro de Saúde"
    )
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview(pois_health, zcol="type")

st_write(pois_health, "/data/IMPT/pois/healthcare.gpkg", delete_dsn = TRUE)

# TODO!
# Pharmacies: Get data source 
# Schools: Use https://github.com/carrismetropolitana/datasets/blob/latest/facilities/schools/schools.csv

# GTFS data ---------------------------------------------------------------

library(GTFShift)
library(lubridate)

gtfs_db = data.frame(
  operator=character(),
  url=character(),
  shapes=logical(),
  outside_area=logical(),
  calendar_add_years=numeric() # For outdated calendars
)

gtfs_db = gtfs_db |> bind_rows(data.frame(
  operator="Carris Metropolitana",
  url="https://api.carrismetropolitana.pt/gtfs",
  shapes=TRUE, outside_area=FALSE, calendar_add_years=NA
))
gtfs_db = gtfs_db |> bind_rows(data.frame(
  operator="Carris Municipal",
  url="https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS",
  shapes=TRUE, outside_area=FALSE, calendar_add_years=NA
))
gtfs_db = gtfs_db |> bind_rows(data.frame(
  operator="Comboios de Portugal",
  url="https://publico.cp.pt/gtfs/gtfs.zip",
  shapes=FALSE, outside_area=TRUE, calendar_add_years=NA
))
gtfs_db = gtfs_db |> bind_rows(data.frame(
  operator="MobiCascais",
  url="https://drive.google.com/u/0/uc?id=13ucYiAJRtu-gXsLa02qKJrGOgDjbnUWX&export=download",
  shapes=TRUE, outside_area=FALSE, calendar_add_years=NA
))
gtfs_db = gtfs_db |> bind_rows(data.frame(
  operator="Metropolitano de Lisboa",
  url="https://www.metrolisboa.pt/google_transit/googleTransit.zip",
  shapes=TRUE, outside_area=FALSE, calendar_add_years=NA
))
gtfs_db = gtfs_db |> bind_rows(data.frame(
  operator="Transtejo Soflusa",
  url="https://api.transtejo.pt/files/GTFS.zip",
  shapes=TRUE, outside_area=FALSE, calendar_add_years=NA
))
gtfs_db = gtfs_db |> bind_rows(data.frame(
  operator="Fertagus",
  url="https://www.fertagus.pt/GTFSTMLzip/Fertagus_GTFS.zip",
  shapes=TRUE, outside_area=FALSE, calendar_add_years=NA
))
gtfs_db = gtfs_db |> bind_rows(data.frame(
  operator="Transportes Colectivos do Barreiro",
  url="https://www.tcbarreiro.pt/front/files/sample_gtfs/GTFS-TCB_24.zip?68960872ed168",
  shapes=TRUE, outside_area=FALSE, calendar_add_years=NA
))
gtfs_db = gtfs_db |> bind_rows(data.frame(
  operator="Metro Transportes do Sul",
  url="https://mts.pt/imt/MTS-20240129.zip",
  shapes=TRUE, outside_area=FALSE, calendar_add_years=1
))

write.csv(gtfs_db, "useful_data/gtfs_db.csv", row.names = FALSE)


gtfs_to_aggregate = list()
for(i in 1:nrow(gtfs_db)){
  gtfs = gtfs_db[i, ]
  operator = gtfs$operator
  message(sprintf("Importing GTFS for %s...", operator))
  
  gtfs_imported = GTFShift::load_feed(gtfs$url, create_transfers=FALSE)
  
  # Write original GTFS
  tidytransit::write_gtfs(gtfs_imported, paste0("/data/IMPT/gtfs/original/gtfs_", gsub(" ", "_", tolower(operator)), ".zip"))
  
  # No need to fix shapes, as GTFShift::load_feed does that by default
  # if (gtfs_db$shapes[i] == FALSE) 
  
  if (gtfs$outside_area == TRUE) {
    message("Feed outside area, filtering...")
    gtfs_imported = tidytransit::filter_feed_by_area(
      gtfs_imported, 
      st_bbox(municipios_union)
    )
  }
  
  if (!is.na(gtfs$calendar_add_years)) {
    message("Feed with outdated calendar, updating...")
    gtfs_imported$calendar = gtfs_imported$calendar |> mutate(
      start_date = date(start_date)+years(gtfs$calendar_add_years),
      end_date = date(end_date)+years(gtfs$calendar_add_years)
    )
    
    # If gtfs_imported$calendar_dates, also update
    if ("calendar_dates" %in% names(gtfs_imported)) {
      gtfs_imported$calendar_dates = gtfs_imported$calendar_dates |> mutate(
        date = date(date)+years(gtfs$calendar_add_years)
      )
    }
    gtfs_imported = tidytransit::as_tidygtfs(gtfs_imported)
  }
  
  gtfs_to_aggregate = append(gtfs_to_aggregate, list(gtfs_imported))
  
  tidytransit::write_gtfs(gtfs_imported, paste0("/data/IMPT/gtfs/processed/gtfs_", gsub(" ", "_", tolower(operator)), ".zip"))
  tidytransit::write_gtfs(gtfs_imported, paste0("/data/IMPT/geo/r5r/gtfs_", gsub(" ", "_", tolower(operator)), ".zip"))
}


# DEM elevation -----------------------------------------------------------

# useful for r5r routing with elevation data, in particular for walking and cycling
# followed the Coipernicus DEM instructions at https://u-shift.github.io/Traffic-Simulation-Models/network.html#elevation

# check DEM
dem = terra::rast("/data/IMPT/geo/r5r/GLPS_DEM_COPERNICUS_30_DEM_2026.tif") # rename the extension to .tif !!
terra::plot(dem)
terra::res(dem) # 30m resolution
terra::crs(dem) # WGS84

# r5r ---------------------------------------------------------------------

# Addapted from https://u-shift.github.io/Traffic-Simulation-Models/network.html

# # r5r dev version with latest features
# utils::remove.packages('r5r')
# devtools::install_github("ipeaGIT/r5r", subdir = "r-package")

# Load packages
library(tidyverse)
library(sf)
options(java.parameters = '-Xmx30G') # allocate memory for 8GB 
library(r5r)

data_path= "/data/IMPT/geo/r5r"
network = r5r::build_network(
  data_path,
  verbose = FALSE,
  elevation = "MINETTI" # optional. TOBLER or NONE
)

# piggyback::pb_upload("/data/IMPT/geo/r5r/network.dat", tag = "latest")



transit_net = transit_network_to_sf(network)
mapview::mapview(transit_net$routes, zcol = "mode")

origins <- st_sf(
  id = "seixal",
  geometry = st_sfc(
    # st_point(c(-9.098720, 38.642105)), # Perto barco (TTSL)
    st_point(c(-9.101199, 38.610285)), # Fogueteiro (Fertagus + Carris)
    # st_point(c(-9.099113, 38.620695)), # Arrentela
    # st_point(c(-8.894169, 38.525378)), # Setúbal (Carris Metropolitana)
    # st_point(c(-9.331880, 38.943850)), # Mafra (Carris Metropolitana)
    # st_point(c(-9.070170, 38.866450)), # Santa iria (CP)
    # st_point(c(-9.446505, 38.711528)), # Cascais (Mobicascais + CP)
    # st_point(c(-9.054498, 38.650430)), # Barreiro (TCB + CP)
    # st_point(c(-9.152688, 38.682233)), # Almada centro (Carris Metriopolitana + Ferry)
    crs = 4326
  )
)

destinations <- st_sf(
  id = "ist",
  geometry = st_sfc(
    st_point(c(-9.139527, 38.738244)), # IST
    # st_point(c(-9.151366, 38.636913)), # Corroios (MTS na ligação a Almada)
    crs = 4326
  )
)


departure_datetime = as.POSIXct("03-02-2026 06:00:00", format = "%d-%m-%Y %H:%M:%S")
detailed_transit = detailed_itineraries(
  r5r_network = network,
  origins = origins,
  destinations = destinations,
  mode = "TRANSIT",
  mode_egress = "WALK",
  departure_datetime = departure_datetime,
  max_rides = 4, # 1 transfers
  # time_window = 10, # the default
  max_walk_time = 30,
  max_trip_duration = 120,
  # drop_geometry = TRUE, # no geometry this time (processig time)
  verbose = FALSE
)

table(detailed_transit$mode)
# View(detailed_transit)
mapview::mapview(detailed_transit, zcol = "mode")

# Grid --------------------------------------------------------------------

# Keep using the TML grid, or produce a new one, replicable, with h3?
# https://u-shift.github.io/Traffic-Simulation-Models/pois.html#hexagonal-using-h3jsr

grelha_tml = sf::st_read("/data/IMPT/BaseDados_PMMUS/Grelha/GrelhaHexagAML/GrelhaHexagAML.shp")
mapview(grelha_tml)
nrow(grelha_tml)
grelha_tml_centroids = st_centroid(grelha_tml)

grelha_tml_cropped = grelha_tml |>
  st_transform(crs=4326) |>
  # Filter polygons inside limit$geom, removing those that are only partially inside (<50%)
  st_intersection(limit$geom) |>
  mutate(area_ha = as.numeric(st_area(geometry))/10000) |>
  filter(area_ha >= 0.5 * (as.numeric(st_area(st_transform(grelha_tml[1, ], crs=4326)))/10000) ) |> # at least 50% of original area  
  st_drop_geometry() |>
  left_join(grelha_tml |> select(id, geometry) |> st_transform(crs=4326), by="id") |> # keep original geometry
  st_as_sf(crs=4326)
  

nrow(grelha_tml_cropped)
# mapview(grelha_tml_cropped)
st_write(grelha_tml_cropped |> select(id), "/data/IMPT/geo/grelha_tml_d500.gpkg", delete_dsn = TRUE)

# We will keep using the TML grid for now, but in the future we can produce a new one with h3, which is replicable and has nice properties (equal area, hierarchical, etc).
library(h3jsr)
# 
# Resolution: https://h3geo.org/docs/core-library/restable/
# h3_res = 10 # 150m diameter
# h3_res = 9 # 400m diameter
h3_res = 8 # 1060m diameter - use for the MVP

GRID_h3 = limit |>
  polygon_to_cells(res = h3_res, simple = FALSE)
GRID_h3 = GRID_h3$h3_addresses |>
  cell_to_polygon(simple = FALSE)
nrow(GRID_h3)
# mapview(GRID_h3) + mapview(grelha_tml_centroids |> st_transform(crs=4326), col.regions="red")

GRID_h3 = GRID_h3 |>
  mutate(id = seq(1:nrow(GRID_h3)))  # give an ID to each cell
h3_index = GRID_h3 |> st_drop_geometry() # save h3_address for later

mapview(GRID_h3)

st_write(GRID_h3 |> select(id), "/data/IMPT/geo/grelha_h3_r8.gpkg", delete_dsn = TRUE)
saveRDS(h3_index, "/data/IMPT/geo/grelha_h3_r8_index.Rds")
# st_write(GRID_h3 |> select(id), "/data/IMPT/geo/grelha_h3_r9.gpkg", delete_dsn = TRUE)
# saveRDS(h3_index, "/data/IMPT/geo/grelha_h3_r9_index.Rds")
GRID_h3_9 = st_read("/data/IMPT/geo/grelha_h3_r9.gpkg")


# # Hex manual
# GRID = limit |>
#   st_transform(crs = 3857) |> # to a projected crs
#   st_make_grid(cellsize = 577, # meters, we are using a projected crs
#                what = "polygons",
#                square = FALSE) |> # if FALSE, hexagons
#   st_sf() |> # from list to sf
#   st_transform(crs = 4326) |>  # back to WGS84
#   st_intersection(limit$geom) # crop (optional)
# nrow(GRID)

# GRID = GRID |>
#   rename(geometry = st_make_grid.st_transform.city_limit..crs...3857...cellsize...400..) |>
#   mutate(id = c(1:nrow(GRID))) # just to give an ID to each cell

# mapview(GRID, alpha.regions = 0.2)+ mapview(grelha_tml_centroids |> st_transform(crs=4326), col.regions="red")



## Grid centroids

GRID_h3_centroids = st_centroid(GRID_h3) |> 	
  select(id, h3_address)

st_write(GRID_h3_centroids, "/data/IMPT/geo/grelha_h3_r8_centroids.gpkg", delete_dsn = TRUE)
# st_write(GRID_h3_centroids, "/data/IMPT/geo/grelha_h3_r9_centroids.gpkg", delete_dsn = TRUE)
