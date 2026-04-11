library(sf)
library(tidyverse)
library(mapview)

# source: geo2.dgterritorio.gov.pt/cos/S2/COS2023/COS2023v1-S2-gpkg.zip
cos_raw = st_read("/media/rosa/Dados/GIS/CAOP/COS/COS2023v1-S2.gpkg")
unique(cos_raw$COS23_n4_L)
unique(cos_raw$COS23_n4_C)

cos_residential = cos_raw |> filter(COS23_n4_L %in% c(
  "Áreas edificadas residenciais contínuas predominantemente verticais",
  "Áreas edificadas residenciais contínuas predominantemente horizontais",
  "Áreas edificadas residenciais descontínuas",
  "Áreas edificadas residenciais descontínuas esparsas"
))

mapview(cos_residential)

# filter aml
municipios ?