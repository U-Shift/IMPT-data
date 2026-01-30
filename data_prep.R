# CAOP 2024 para  atualizar as NUT II

library(sf)
library(dplyr)
library(mapview)

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

# mapview(CAOP_GLPS)
# View(CAOP_GLPS)

st_write(CAOP_GLPS, "/data/IMPT/geo/freguesias_2024.gpkg", delete_dsn = TRUE)

# group sf by municipio
municipios = CAOP_GLPS |> 
  group_by(municipio) |>
  summarise(geometry = st_union(geom)) |>
  st_transform(crs = 4326)
municipios = municipios [-6, ] # Remove strange Lisbon

# mapview(municipios)
st_write(municipios, "/data/IMPT/geo/municipios_2024.gpkg", delete_dsn = TRUE)
