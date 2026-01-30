# CAOP 2024 para  atualizar as NUT II

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

# group sf by municipio
municipios = CAOP_GLPS |> 
  group_by(municipio) |>
  summarise(geometry = st_union(geom)) |>
  st_transform(crs = 4326)
municipios = municipios [-6, ] # Remove strange Lisbon

# mapview(municipios)
st_write(municipios, "/data/IMPT/geo/municipios_2024.gpkg", delete_dsn = TRUE)

municipios_union = municipios |> sf::st_union() |> sf::st_make_valid()
st_write(municipios_union, "/data/IMPT/geo/municipios_union_2024.geojson", delete_dsn = TRUE)


# OSM data ----------------------------------------------------------------

library(osmdata)

# Road network exported using Hot Exports Tool, https://export.hotosm.org/exports/4782f0b8-6778-4c0e-8e4f-97fc62e7f240, to generate .pbf file for r5r
road_network = st_read("/data/IMPT/geo/IMPT_Road_network.gpkg")


# Trips -------------------------------------------------------------------

# trips_freguesias_2011 = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/TRIPSmode_freguesias.Rds"))
trips_freguesias_2016 = readRDS(url("https://github.com/U-Shift/MQAT/raw/refs/heads/main/data/TRIPSmode_freg.Rds"))

FREGUESIASgeo_2016 = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/FREGUESIASgeo.Rds"))
trips_freguesias_2016_sf = trips_freguesias_2016 |> 
  select(Origin_dicofre16) |>
  distinct() |>
  left_join(FREGUESIASgeo_2016 |> select(Dicofre), by=c("Origin_dicofre16"="Dicofre")) |>
  st_as_sf(crs=4326)


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

write.csv(conversion_dicofre, "useful_data/dicofre_16_24_conversion.csv")


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



