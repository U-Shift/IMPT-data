# aim: compare with available data from https://www.ipea.gov.br/acessooportunidades/mapa/

library(sf)
library(dplyr)

# grid_ipea_url = "https://www.ipea.gov.br/geobr/aopdata/data/website_data/aop_hex_grid_v2.gpkg"
# download.file(grid_ipea_url, "/data/IMPT/test/aop_hex_grid_v2.gpkg", mode = "wb")
grid_ipea = st_read("/data/IMPT/test/aop_hex_grid_v2.gpkg")


# eles primeiro fizeram uma grelha com a informação da quantidade de todas as oportunidades por tipo
# depois calcularam o TTM (com tempo mínimo!! inf quando > 2h carro ou TP, >1.5 W ou B)
# para Walk e Bike (sem ter em conta o período semanal), e para Car e TP com HP e FHP
# tendo já as viagens calculadas para os diferentes modos, depois foi só calcular a acessibilidade acumulada,
# e o tempo médio ao equipamento mais próximo
# tendo usado como cutoff:
# - 15: W e B
# - 30: C, TP, W, B
# - 45: W e B
# - 60: C, TP, W, B
# - 90: C e TP
# - 120: C e TP
# fazem ainda distinção entre o cumulativo ativo e o passivo
# ativo: quandidade de educação, saúde, lazer, comércio e serviços
# passivo: quantidade de pessoas, mulheres, crianças, idosos



# ver dados finais acessibilidade walk e bike
data_ipea_url = "https://www.ipea.gov.br/geobr/aopdata/data/website_data/aop_access_active_2019_v2.csv" 
download.file(data_ipea_url, "/data/IMPT/test/aop_access_active_2019_v2.csv", mode = "wb")
data_ipea_WalkBike = read.csv("/data/IMPT/test/aop_access_active_2019_v2.csv")
names(data_ipea_WalkBike)
# [1] "year"        "id_hex"      "abbrev_muni" "name_muni"   "code_muni"   "mode"        "peak"       
# [8] "CMATT15"     "CMATB15"     "CMATM15"     "CMATA15"     "CMAST15"     "CMASB15"     "CMASM15"    
# [15] "CMASA15"     "CMAET15"     "CMAEI15"     "CMAEF15"     "CMAEM15"     "CMAMT15"     "CMAMI15"   
# ... [140]

# see metadata: https://ipeagit.github.io/aopdata/articles/data_dic_pt.html


# dados uso do solo
data_ipea_landuse = read.csv("/data/IMPT/test/aop_landuse_2019_v2.csv")
names(data_ipea_landuse)
# [1] "year"        "id_hex"      "abbrev_muni" "name_muni"   "code_muni"   "T001"        "T002"       
# [8] "T003"        "T004"        "E001"        "E002"        "E003"        "E004"        "M001"       
# [15] "M002"        "M003"        "M004"        "S001"        "S002"        "S003"        "S004"       
# [22] "C001"       
# este inclui trabalho, escolas, matrículas escolares, saúde, assistencia social


# dados census e rendimento
data_ipea_demographic = read.csv("/data/IMPT/test/aop_population_2010_v2.csv")
names(data_ipea_demographic)
# [1] "year"        "id_hex"      "abbrev_muni" "name_muni"   "code_muni"   "P001"        "P002"       
# [8] "P003"        "P004"        "P005"        "P006"        "P007"        "P010"        "P011"       
# [15] "P012"        "P013"        "P014"        "P015"        "P016"        "R001"        "R002"       
# [22] "R003"       
# este inclui população total, mulheres, crianças, idosos, pessoas com deficiência, renda per capita média,
# quintil de renda, decil de renda


# see https://ipeagit.github.io/aopdata/articles/aopdata.html
# for functions to explore and visualize maps

# devtools::install_github("ipeaGIT/aopdata", subdir = "r-package")
library(aopdata)
aopdata_dictionary(lang = 'pt') # view dictionary

# download acessibility data for Curitiba, public transport, peak hours, 2019, with geometry
data_ipea_example <- aopdata::read_access(  # NOT WORKING DIRECLTY
  city = 'Curitiba', 
  mode = 'public_transport', 
  peak = TRUE,
  year = 2019,
  geometry = TRUE
)
names(data_ipea_example)

# see also https://ipeagit.github.io/aopdata/articles/access_inequality.html
# and https://ipeagit.github.io/aopdata/articles/landuse_maps.html
# and https://ipeagit.github.io/aopdata/articles/population_maps.html
