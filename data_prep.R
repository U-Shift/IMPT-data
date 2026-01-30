# CAOP 2024 para  atualizar as NUT II

library(sf)
library(dplyr)

# dowload and extract with sf
link_DGT = "https://geo2.dgterritorio.gov.pt/caop/CAOP_Continente_2024_1-gpkg.zip"
temp <- "/data/IMPT/original/caop2024.zip"
download.file(link_DGT, temp, mode = "wb")
unzip(temp, exdir = "/data/IMPT/original/")
CAOP_PT = st_read("/data/IMPT/original/Continente_CAOP2024_1.gpkg")

# selecionar apenas NUT II Lisboa
CAOP_GLPS = CAOP_PT |> filte