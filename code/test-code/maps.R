library(mapview)


# Color Palletes ----------------------------------------------------------


RedToGreen = c("#a50026", "#a70226", "#a90426", "#ab0626", "#ad0826", "#af0926", "#b10b26", "#b30d26", "#b50f26", "#b61127", "#b81327", "#ba1527", "#bc1727", "#be1927", "#c01b27", "#c21d28", "#c41f28", "#c52128", "#c72328", "#c92529", "#cb2729", "#cc2929", "#ce2b2a", "#d02d2a", "#d12f2b", "#d3312b", "#d4332c", "#d6352c", "#d7382d", "#d93a2e", "#da3c2e", "#dc3e2f", "#dd4030", "#de4331", "#e04532", "#e14733", "#e24a33", "#e34c34", "#e44e35", "#e55136", "#e75337", "#e85538", "#e95839", "#ea5a3a", "#eb5d3c", "#ec5f3d", "#ed613e", "#ed643f", "#ee6640", "#ef6941", "#f06b42", "#f16e43", "#f17044", "#f27346", "#f37547", "#f37848", "#f47a49", "#f57d4a", "#f57f4b", "#f6824d", "#f6844e", "#f7864f", "#f78950", "#f88b51", "#f88e53", "#f89054", "#f99355", "#f99556", "#f99858", "#fa9a59", "#fa9c5a", "#fa9f5b", "#fba15d", "#fba35e", "#fba660", "#fba861", "#fcaa62", "#fcad64", "#fcaf65", "#fcb167", "#fcb368", "#fcb56a", "#fdb86b", "#fdba6d", "#fdbc6e", "#fdbe70", "#fdc071", "#fdc273", "#fdc474", "#fdc676", "#fdc878", "#fdca79", "#fecc7b", "#fecd7d", "#fecf7e", "#fed180", "#fed382", "#fed584", "#fed685", "#fed887", "#feda89", "#fedb8b", "#fedd8d", "#fede8f", "#fee090", "#fee192", "#fee394", "#fee496", "#fee698", "#fee79a", "#fee89b", "#feea9d", "#feeb9f", "#feeca0", "#feeda2", "#feeea3", "#fdefa5", "#fdf0a6", "#fdf1a7", "#fdf2a9", "#fcf3aa", "#fcf4ab", "#fcf5ab", "#fbf5ac", "#fbf6ad", "#faf6ad", "#faf7ad", "#f9f7ae", "#f8f7ae", "#f7f8ad", "#f7f8ad", "#f6f8ad", "#f5f8ac", "#f4f8ab", "#f3f8ab", "#f1f8aa", "#f0f7a9", "#eff7a8", "#eef7a6", "#edf6a5", "#ebf6a4", "#eaf6a2", "#e8f5a1", "#e7f59f", "#e6f49d", "#e4f39c", "#e2f39a", "#e1f298", "#dff297", "#def195", "#dcf093", "#daef92", "#d9ef90", "#d7ee8e", "#d5ed8d", "#d3ec8b", "#d2ec89", "#d0eb88", "#ceea86", "#cce985", "#cae983", "#c8e882", "#c6e780", "#c4e67f", "#c2e57e", "#c0e47c", "#bee47b", "#bce37a", "#bae279", "#b8e178", "#b6e076", "#b4df75", "#b2de74", "#b0dd73", "#aedc72", "#acdb71", "#a9da70", "#a7d970", "#a5d86f", "#a3d86e", "#a0d76d", "#9ed66c", "#9cd56c", "#99d36b", "#97d26b", "#95d16a", "#92d069", "#90cf69", "#8ece68", "#8bcd68", "#89cc67", "#86cb67", "#84ca66", "#81c966", "#7fc866", "#7cc665", "#79c565", "#77c464", "#74c364", "#71c263", "#6fc063", "#6cbf62", "#69be62", "#67bd62", "#64bc61", "#61ba60", "#5eb960", "#5cb85f", "#59b65f", "#56b55e", "#53b45e", "#51b25d", "#4eb15c", "#4baf5c", "#48ae5b", "#46ad5a", "#43ab5a", "#40aa59", "#3da858", "#3ba757", "#38a557", "#36a456", "#33a255", "#31a154", "#2e9f54", "#2c9d53", "#2a9c52", "#289a51", "#259950", "#23974f", "#21954f", "#1f944e", "#1e924d", "#1c904c", "#1a8f4b", "#188d4a", "#178b49", "#158948", "#148747", "#128646", "#118446", "#108245", "#0e8044", "#0d7e43", "#0c7d42", "#0b7b41", "#0a7940", "#08773f", "#07753e", "#06733d", "#05713c", "#04703b", "#036e3a", "#026c39", "#016a38", "#006837")

GreenToRed = rev(RedToGreen)

WhiteToBlue = c('#f7fbff', '#f6faff', '#f5fafe', '#f5f9fe', '#f4f9fe', '#f3f8fe', '#f2f8fd', '#f2f7fd', '#f1f7fd', '#f0f6fd', '#eff6fc', '#eef5fc', '#eef5fc', '#edf4fc', '#ecf4fb', '#ebf3fb', '#eaf3fb', '#eaf2fb', '#e9f2fa', '#e8f1fa', '#e7f1fa', '#e7f0fa', '#e6f0f9', '#e5eff9', '#e4eff9', '#e3eef9', '#e3eef8', '#e2edf8', '#e1edf8', '#e0ecf8', '#dfecf7', '#dfebf7', '#deebf7', '#ddeaf7', '#dceaf6', '#dce9f6', '#dbe9f6', '#dae8f6', '#d9e8f5', '#d9e7f5', '#d8e7f5', '#d7e6f5', '#d6e6f4', '#d6e5f4', '#d5e5f4', '#d4e4f4', '#d3e4f3', '#d3e3f3', '#d2e3f3', '#d1e2f3', '#d0e2f2', '#d0e1f2', '#cfe1f2', '#cee0f2', '#cde0f1', '#cddff1', '#ccdff1', '#cbdef1', '#cadef0', '#caddf0', '#c9ddf0', '#c8dcf0', '#c7dcef', '#c7dbef', '#c6dbef', '#c4daee', '#c3daee', '#c2d9ee', '#c1d9ed', '#bfd8ed', '#bed8ec', '#bdd7ec', '#bcd7eb', '#bad6eb', '#b9d6ea', '#b8d5ea', '#b7d4ea', '#b5d4e9', '#b4d3e9', '#b3d3e8', '#b2d2e8', '#b0d2e7', '#afd1e7', '#aed1e7', '#add0e6', '#abd0e6', '#aacfe5', '#a9cfe5', '#a8cee4', '#a6cee4', '#a5cde3', '#a4cce3', '#a3cce3', '#a1cbe2', '#a0cbe2', '#9fcae1', '#9dcae1', '#9cc9e1', '#9ac8e0', '#99c7e0', '#97c6df', '#95c5df', '#94c4df', '#92c4de', '#91c3de', '#8fc2de', '#8dc1dd', '#8cc0dd', '#8abfdd', '#89bedc', '#87bddc', '#85bcdc', '#84bcdb', '#82bbdb', '#81badb', '#7fb9da', '#7db8da', '#7cb7da', '#7ab6d9', '#79b5d9', '#77b5d9', '#75b4d8', '#74b3d8', '#72b2d8', '#71b1d7', '#6fb0d7', '#6dafd7', '#6caed6', '#6aaed6', '#69add5', '#68acd5', '#66abd4', '#65aad4', '#64a9d3', '#63a8d3', '#61a7d2', '#60a7d2', '#5fa6d1', '#5da5d1', '#5ca4d0', '#5ba3d0', '#5aa2cf', '#58a1cf', '#57a0ce', '#56a0ce', '#549fcd', '#539ecd', '#529dcc', '#519ccc', '#4f9bcb', '#4e9acb', '#4d99ca', '#4b98ca', '#4a98c9', '#4997c9', '#4896c8', '#4695c8', '#4594c7', '#4493c7', '#4292c6', '#4191c6', '#4090c5', '#3f8fc5', '#3e8ec4', '#3d8dc4', '#3c8cc3', '#3b8bc2', '#3a8ac2', '#3989c1', '#3888c1', '#3787c0', '#3686c0', '#3585bf', '#3484bf', '#3383be', '#3282be', '#3181bd', '#3080bd', '#2f7fbc', '#2e7ebc', '#2d7dbb', '#2c7cba', '#2b7bba', '#2a7ab9', '#2979b9', '#2777b8', '#2676b8', '#2575b7', '#2474b7', '#2373b6', '#2272b6', '#2171b5', '#2070b4', '#206fb4', '#1f6eb3', '#1e6db2', '#1d6cb1', '#1c6bb0', '#1c6ab0', '#1b69af', '#1a68ae', '#1967ad', '#1966ad', '#1865ac', '#1764ab', '#1663aa', '#1562a9', '#1561a9', '#1460a8', '#135fa7', '#125ea6', '#125da6', '#115ca5', '#105ba4', '#0f5aa3', '#0e59a2', '#0e58a2', '#0d57a1', '#0c56a0', '#0b559f', '#0a549e', '#0a539e', '#09529d', '#08519c', '#08509b', '#084f99', '#084e98', '#084d96', '#084c95', '#084b93', '#084a91', '#084990', '#08488e', '#08478d', '#08468b', '#08458a', '#084488', '#084387', '#084285', '#084184', '#084082', '#083e81', '#083d7f', '#083c7d', '#083b7c', '#083a7a', '#083979', '#083877', '#083776', '#083674', '#083573', '#083471', '#083370', '#08326e', '#08316d', '#08306b')

BlueToWhite = rev(WhiteToBlue)

Viridis = c("#440154", "#440256", "#450457", "#450559", "#46075a", "#46085c", "#460a5d", "#460b5e", "#470d60", "#470e61", "#471063", "#471164", "#471365", "#481467", "#481668", "#481769", "#48186a", "#481a6c", "#481b6d", "#481c6e", "#481d6f", "#481f70", "#482071", "#482173", "#482374", "#482475", "#482576", "#482677", "#482878", "#482979", "#472a7a", "#472c7a", "#472d7b", "#472e7c", "#472f7d", "#46307e", "#46327e", "#46337f", "#463480", "#453581", "#453781", "#453882", "#443983", "#443a83", "#443b84", "#433d84", "#433e85", "#423f85", "#424086", "#424186", "#414287", "#414487", "#404588", "#404688", "#3f4788", "#3f4889", "#3e4989", "#3e4a89", "#3e4c8a", "#3d4d8a", "#3d4e8a", "#3c4f8a", "#3c508b", "#3b518b", "#3b528b", "#3a538b", "#3a548c", "#39558c", "#39568c", "#38588c", "#38598c", "#375a8c", "#375b8d", "#365c8d", "#365d8d", "#355e8d", "#355f8d", "#34608d", "#34618d", "#33628d", "#33638d", "#32648e", "#32658e", "#31668e", "#31678e", "#31688e", "#30698e", "#306a8e", "#2f6b8e", "#2f6c8e", "#2e6d8e", "#2e6e8e", "#2e6f8e", "#2d708e", "#2d718e", "#2c718e", "#2c728e", "#2c738e", "#2b748e", "#2b758e", "#2a768e", "#2a778e", "#2a788e", "#29798e", "#297a8e", "#297b8e", "#287c8e", "#287d8e", "#277e8e", "#277f8e", "#27808e", "#26818e", "#26828e", "#26828e", "#25838e", "#25848e", "#25858e", "#24868e", "#24878e", "#23888e", "#23898e", "#238a8d", "#228b8d", "#228c8d", "#228d8d", "#218e8d", "#218f8d", "#21908d", "#21918c", "#20928c", "#20928c", "#20938c", "#1f948c", "#1f958b", "#1f968b", "#1f978b", "#1f988b", "#1f998a", "#1f9a8a", "#1e9b8a", "#1e9c89", "#1e9d89", "#1f9e89", "#1f9f88", "#1fa088", "#1fa188", "#1fa187", "#1fa287", "#20a386", "#20a486", "#21a585", "#21a685", "#22a785", "#22a884", "#23a983", "#24aa83", "#25ab82", "#25ac82", "#26ad81", "#27ad81", "#28ae80", "#29af7f", "#2ab07f", "#2cb17e", "#2db27d", "#2eb37c", "#2fb47c", "#31b57b", "#32b67a", "#34b679", "#35b779", "#37b878", "#38b977", "#3aba76", "#3bbb75", "#3dbc74", "#3fbc73", "#40bd72", "#42be71", "#44bf70", "#46c06f", "#48c16e", "#4ac16d", "#4cc26c", "#4ec36b", "#50c46a", "#52c569", "#54c568", "#56c667", "#58c765", "#5ac864", "#5cc863", "#5ec962", "#60ca60", "#63cb5f", "#65cb5e", "#67cc5c", "#69cd5b", "#6ccd5a", "#6ece58", "#70cf57", "#73d056", "#75d054", "#77d153", "#7ad151", "#7cd250", "#7fd34e", "#81d34d", "#84d44b", "#86d549", "#89d548", "#8bd646", "#8ed645", "#90d743", "#93d741", "#95d840", "#98d83e", "#9bd93c", "#9dd93b", "#a0da39", "#a2da37", "#a5db36", "#a8db34", "#aadc32", "#addc30", "#b0dd2f", "#b2dd2d", "#b5de2b", "#b8de29", "#bade28", "#bddf26", "#c0df25", "#c2df23", "#c5e021", "#c8e020", "#cae11f", "#cde11d", "#d0e11c", "#d2e21b", "#d5e21a", "#d8e219", "#dae319", "#dde318", "#dfe318", "#e2e418", "#e5e419", "#e7e419", "#eae51a", "#ece51b", "#efe51c", "#f1e51d", "#f4e61e", "#f6e620", "#f8e621", "#fbe723", "#fde725")

WhiteToRed = c("#fff5f0", "#fff4ef", "#fff4ee", "#fff3ed", "#fff2ec", "#fff2eb", "#fff1ea", "#fff0e9", "#fff0e8", "#ffefe7", "#ffeee6", "#ffeee6", "#ffede5", "#ffece4", "#ffece3", "#ffebe2", "#feeae1", "#fee9e0", "#fee9de", "#fee8dd", "#fee7dc", "#fee6db", "#fee6da", "#fee5d9", "#fee4d8", "#fee3d7", "#fee2d6", "#fee2d5", "#fee1d4", "#fee0d2", "#fedfd1", "#feded0", "#feddcf", "#fedccd", "#fedbcc", "#fedacb", "#fed9ca", "#fed8c8", "#fed7c7", "#fdd6c6", "#fdd5c4", "#fdd4c3", "#fdd3c1", "#fdd2c0", "#fdd1bf", "#fdd0bd", "#fdcfbc", "#fdceba", "#fdcdb9", "#fdccb7", "#fdcbb6", "#fdc9b4", "#fdc8b3", "#fdc7b2", "#fdc6b0", "#fdc5af", "#fdc4ad", "#fdc2ac", "#fdc1aa", "#fdc0a8", "#fcbfa7", "#fcbea5", "#fcbca4", "#fcbba2", "#fcbaa1", "#fcb99f", "#fcb89e", "#fcb69c", "#fcb59b", "#fcb499", "#fcb398", "#fcb196", "#fcb095", "#fcaf94", "#fcae92", "#fcac91", "#fcab8f", "#fcaa8e", "#fca98c", "#fca78b", "#fca689", "#fca588", "#fca486", "#fca285", "#fca183", "#fca082", "#fc9e81", "#fc9d7f", "#fc9c7e", "#fc9b7c", "#fc997b", "#fc987a", "#fc9778", "#fc9677", "#fc9475", "#fc9374", "#fc9273", "#fc9071", "#fc8f70", "#fc8e6f", "#fc8d6d", "#fc8b6c", "#fc8a6b", "#fc8969", "#fc8868", "#fc8667", "#fc8565", "#fc8464", "#fb8263", "#fb8162", "#fb8060", "#fb7f5f", "#fb7d5e", "#fb7c5d", "#fb7b5b", "#fb795a", "#fb7859", "#fb7758", "#fb7657", "#fb7455", "#fa7354", "#fa7253", "#fa7052", "#fa6f51", "#fa6e50", "#fa6c4e", "#f96b4d", "#f96a4c", "#f9684b", "#f9674a", "#f96549", "#f86448", "#f86347", "#f86146", "#f86045", "#f75e44", "#f75d43", "#f75c42", "#f65a41", "#f65940", "#f6573f", "#f5563e", "#f5553d", "#f4533c", "#f4523b", "#f4503a", "#f34f39", "#f34e38", "#f24c37", "#f24b37", "#f14936", "#f14835", "#f04734", "#ef4533", "#ef4433", "#ee4332", "#ed4131", "#ed4030", "#ec3f2f", "#eb3d2f", "#eb3c2e", "#ea3b2d", "#e93a2d", "#e8382c", "#e7372b", "#e6362b", "#e6352a", "#e5342a", "#e43229", "#e33128", "#e23028", "#e12f27", "#e02e27", "#df2d26", "#de2c26", "#dd2b25", "#dc2a25", "#db2924", "#da2824", "#d92723", "#d72623", "#d62522", "#d52422", "#d42321", "#d32221", "#d22121", "#d12020", "#d01f20", "#ce1f1f", "#cd1e1f", "#cc1d1f", "#cb1d1e", "#ca1c1e", "#c91b1e", "#c71b1d", "#c61a1d", "#c5191d", "#c4191c", "#c3181c", "#c2181c", "#c0171b", "#bf171b", "#be161b", "#bd161a", "#bb151a", "#ba151a", "#b91419", "#b81419", "#b61419", "#b51319", "#b41318", "#b21218", "#b11218", "#b01218", "#ae1117", "#ad1117", "#ac1117", "#aa1017", "#a91016", "#a71016", "#a60f16", "#a40f16", "#a30e15", "#a10e15", "#a00e15", "#9e0d15", "#9c0d14", "#9b0c14", "#990c14", "#970c14", "#960b13", "#940b13", "#920a13", "#900a13", "#8f0a12", "#8d0912", "#8b0912", "#890812", "#870811", "#860711", "#840711", "#820711", "#800610", "#7e0610", "#7c0510", "#7a0510", "#78040f", "#76040f", "#75030f", "#73030f", "#71020e", "#6f020e", "#6d010e", "#6b010e", "#69000d", "#67000d")

RedToWhite = rev(WhiteToRed)

select_equidistant <- function(vec, x) {
  n <- length(vec)
  
  # If x is greater than or equal to the length, return the original vector
  if (x >= n) {
    return(vec)
  }
  
  # Generate x indices from 1 to n with equal spacing
  # Using round() ensures we get integer positions
  indices <- seq(from = 1, to = n, length.out = x)
  
  return(vec[round(indices)])
}

# Maps --------------------------------------------------------------------

## 1 Introduction --------------------------------------------------------------------

grid_aggregated <- impt_read("/dashboard_data/grid_aggregated.geojson")
freguesias_aggregated <- impt_read("/dashboard_data/freguesias_aggregated.geojson")
municipios_aggregated <- impt_read("/dashboard_data/municipios_aggregated.geojson")

mapview(
  municipios_aggregated |> select(IMPT_score_pca_geom_pass) |> arrange(IMPT_score_pca_geom_pass), 
  homebutton = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  zcol="IMPT_score_pca_geom_pass",
  col.regions = select_equidistant(GreenToRed, 10),
  layer.name="IMPT Critical"
)

mapview(
  freguesias_aggregated |> select(IMPT_score_pca_geom_pass) |> arrange(IMPT_score_pca_geom_pass), 
  homebutton = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  zcol="IMPT_score_pca_geom_pass",
  col.regions = select_equidistant(GreenToRed, 10),
  layer.name="IMPT Critical"
)

mapview(
  grid_aggregated |> select(IMPT_score_pca_geom_pass) |> arrange(IMPT_score_pca_geom_pass), 
  homebutton = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  zcol="IMPT_score_pca_geom_pass",
  col.regions = select_equidistant(GreenToRed, 10),
  layer.name="IMPT Critical"
)

freguesias <- impt_read("/geo/freguesias_2024_unique.gpkg")
municipios <- impt_read("/geo/municipios_2024.gpkg")
grid <- impt_read("/geo/grelha_h3_r8.gpkg")

mapview(
  grid, 
  homebutton = FALSE, legend = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  col.regions="#015160"
)
mapview(
  freguesias, 
  homebutton = FALSE, legend = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  col.regions="#015160"
)
mapview(
  municipios, 
  homebutton = FALSE, legend = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  col.regions="#015160"
)



## 4. Data preparation -----------------------------------------------------

# CAOP_GLPS_ORIGINAL from 01_data_prep.R
mapview(
  CAOP_GLPS_ORIGINAL,
  # Hide elements, for printscreen
  homebutton = FALSE, legend = FALSE, layer.control.show = FALSE,
  # Color
  col.regions = "#015160"
) + mapview(
  freguesias,
  # Hide elements, for printscreen
  homebutton = FALSE, legend = FALSE, layer.control.show = FALSE,
  # Color
  col.regions = "#015160"
)

# Parish 2016 to 2024 administrative boundaries conversion
mapview(FREGUESIASgeo_2016 |> filter(Dicofre %in% c(151007)), homebutton = FALSE, legend = FALSE, layer.control.show = FALSE, col.regions = "#015160") +
mapview(freguesias |> filter(dtmnfr %in% c(151008, 151009, 151010)), homebutton = FALSE, legend = FALSE, layer.control.show = FALSE, col.regions = "#015160")

# Census Data Processing
mapview(Census21_BGRI, homebutton = FALSE, layer.control.show = FALSE, col.regions = "#015160", layer.name="Census BGRI") + 
  mapview(census_points21, homebutton = FALSE, layer.control.show = FALSE, col.regions = "#FFFFFF", color = "#000000", layer.name="Census BGRI Centroids")

# 4.5 Dasymetric Grid Population Redistribution (COS 2023)
# From 02_census_grid_with_cos.Rith_cos
unique(cos_residential_aml$COS23_n4_L)
cos_residential_aml_map <- cos_residential_aml |> 
  mutate(category = case_when(
    COS23_n4_L == "Áreas edificadas residenciais contínuas predominantemente verticais" ~ "Continuous Vertical",
    COS23_n4_L == "Áreas edificadas residenciais contínuas predominantemente horizontais" ~ "Continuous Horizontal",
    COS23_n4_L == "Áreas edificadas residenciais descontínuas" ~ "Discontinuous",
    COS23_n4_L == "Áreas edificadas residenciais descontínuas esparsas" ~ "Sparse Discontinuous",
    TRUE ~ COS23_n4_L
  ))
unique(cos_residential_aml_map$category)
mapview(cos_residential_aml_map, zcol = "category", homebutton = FALSE, layer.control.show = FALSE, col.regions=c("#e31a1c", "#800026", "#fd8d3c", "#fed976"), layer.name="COS 2023 classification")


# Points of interest
pois_health <- impt_read("/pois/healthcare.gpkg")

mapview(municipios, alpha.regions=0, color="#000000", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) + 
mapview(pois_health, homebutton = FALSE, layer.control.show = FALSE, layer.name="Health POIs", zcol="type", col.regions=c("#015160", "#a1dab4"))


# Jobs jittering
buildings <- impt_read("/pois/lisbon_metro_buildings_height.geojson")
buildings <- buildings |>
  mutate(volume = height * footprint_m2) |> # volume
  mutate(weight = round(volume, digits=0))

road_osm = sf::st_read("https://github.com/U-Shift/GTFShift/releases/download/v0.7.0/osm_arroios.gpkg")
pois_jobs_buildings <- impt_read("/pois/pois_jobs_imob_jt50_buildings.gpkg") |> rename(n = trips)

freguesia = freguesias |> filter(freguesia=="Areeiro")
mapview(freguesia)
buildings_freguesia = buildings |> st_intersection(sf::st_as_sfc(freguesia))
pois_jobs_buildings_freguesia <- pois_jobs_buildings |> st_intersection(sf::st_as_sfc(freguesia))


mapview(freguesia, alpha.regions=0, color="#000000", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) + 
mapview(
  buildings_freguesia |> select(weight), # |> filter(weight<quantile(buildings_cascais$weight, 0.95)),
  homebutton = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  zcol="weight",
  col.regions = select_equidistant(WhiteToBlue, 10),
  layer.name="Buildings volume",
  hide=TRUE
) + mapview(
  pois_jobs_buildings_freguesia |> mutate(n = round(n)) |> select(n),
  homebutton = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  zcol="n",
  col.regions = select_equidistant(WhiteToBlue, 10),
  layer.name="Jittered jobs",
  hide=TRUE
)


# Road Network and DEM

dem <- terra::rast("/data/IMPT/geo/r5r/GLPS_DEM_COPERNICUS_30_DEM_2026.tif") # rename the extension to .tif !!
terra::plot(dem)

road_network <- impt_read("/geo/IMPT_Road_network.gpkg")
names(road_network)


road_network_main <- road_network |>
  filter(highway %in% c("primary", "secondary", "tertiary", "trunk", "motorway")) |>
  select(osm_id, name, highway)

mapview(municipios, alpha.regions=0, color="#000000", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) + 
  mapview(
    road_network_main |> select(osm_id, highway), 
    zcol="highway", 
    homebutton = FALSE, layer.control.show = FALSE, 
    layer.name="Main road network",
    col.regions=select_equidistant(Viridis, 5)
  )

# Census modal share
census_modal_share <- impt_read("census2021/census_modal_share_parish.csv") |> mutate(dtmnfr = as.character(dtmnfr)) |>
  left_join(freguesias |> select(dtmnfr, geom), by="dtmnfr") |>
  mutate(
    across(contains("share"), ~ round(.x * 100, digits=0))
  ) |>
  st_as_sf()
names(census_modal_share)

mapview(census_modal_share, zcol="pt_share", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue, 10), layer.name="Public transport modal share") + 
  mapview(census_modal_share, zcol="private_vehicle_share", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue, 10), layer.name="Car modal share", hide=TRUE) + 
  mapview(census_modal_share, zcol="walk_share", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue, 10), layer.name="Walk modal share", hide=TRUE) + 
  mapview(census_modal_share, zcol="bike_share", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue, 10), layer.name="Bike modal share", hide=TRUE)


# Vehicle ownership
vehicles_by_parish <- impt_read("imob/imob_vehicles_freg.csv") |>
  mutate(dicofre = as.character(dicofre)) |>
  left_join(freguesias |> select(dtmnfr, geom), by=c("dicofre" = "dtmnfr")) |>
  st_as_sf()   

vehicles_by_parish$total_motor_vehicles_per_hh
vehicles_by_parish$pct_hh_no_vehicle

mapview(vehicles_by_parish, zcol="total_motor_vehicles_per_hh", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis, 10), layer.name="Total motor vehicles per household") + 
  mapview(vehicles_by_parish, zcol="pct_hh_no_vehicle", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis, 10), layer.name="% households with no vehicle", hide=TRUE)



## 5  Travel Time Matrix ---------------------------------------------------

ttm_car_60min_202602040300 <- impt_read("ttm/ttm_h3_res8/ttm_car_60min_202602040300.rds")
points_h3 <- impt_read("/geo/grelha_h3_r8_centroids.gpkg")

ttm_map = ttm_car_60min_202602040300 |>
  sample_n(5000) |>
  left_join(points_h3 |> rename(from_geom = geom), by=c("from_id" = "id")) |>
  left_join(points_h3 |> rename(to_geom = geom), by=c("to_id" = "id")) |>
  # Create line between from and to
  rowwise() |>
  mutate(geometry = st_cast(st_union(from_geom, to_geom), "LINESTRING")) |>
  select(-from_geom, -to_geom)

ttm_map <- st_as_sf(ttm_map)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
mapview(ttm_map, alpha=0.5, zcol="travel_time_p50", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis, 10), layer.name="Travel time (min)")

freguesias_centroids = freguesias |> sf::st_centroid()
buildings <- impt_read("/pois/lisbon_metro_buildings_height.geojson")

mapview(ttm_map, alpha=0.5, zcol="travel_time_p50", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis, 10), layer.name="Jittered O/D") +
  mapview(freguesias_centroids, homebutton = FALSE, layer.control.show = FALSE, layer.name="Original O/D", color="black", col.regions="gray", lwd=1)
  # mapview(buildings, homebutton = FALSE, layer.control.show = FALSE, layer.name="Buildings", color="orange", col.regions="orange", lwd=1) 



## 6  Mobility -------------------------------------------------------------

# 6.1 Commuting travel times
grid_commuting <- impt_read("/mobility_commuting/grid_commuting.csv") |>
  left_join(grid, by=c("id_grid_origin" = "id")) |>
  st_as_sf()

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
mapview(grid_commuting, zcol="avg_tt_transit_2t_120m_15w", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(GreenToRed, 10), layer.name="Commuting travel time by PT (min)") + 
mapview(grid_commuting, zcol="avg_tt_car", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(GreenToRed, 10), layer.name="Commuting travel time by car (min)", hide=TRUE)

# 6.2 Number of PT transfers
grid_transfers <- impt_read("/mobility_commuting/grid_transfers.csv") |>
  left_join(grid, by=c("id_grid_origin" = "id")) |>
  st_as_sf() |>
  filter(!is.na(weighted_mean_transfers))

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_transfers, zcol="weighted_mean_transfers", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(GreenToRed, 10), layer.name="Average number of transfers")


# 6.3 Walking and Cycling infrastructure ratio
grid_mobility_infrastructure <- impt_read("/mobility/grid_infrastructure_ratio.csv") |>
  left_join(grid, by=c("id" = "id")) |>
  st_as_sf() |>
  mutate(pedpath_to_road_ratio = pedpath_to_road_ratio*100, cycleway_to_road_ratio = cycleway_to_road_ratio*100, cycling_quality_ratio=cycling_quality_ratio*100)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_mobility_infrastructure |> filter(pedpath_to_road_ratio < quantile(pedpath_to_road_ratio, 0.99, na.rm=TRUE)), zcol="pedpath_to_road_ratio", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue, 10), layer.name="Pedestrian path to road ratio (%)") +
  mapview(grid_mobility_infrastructure |> filter(cycleway_to_road_ratio < quantile(cycleway_to_road_ratio, 0.99, na.rm=TRUE)), zcol="cycleway_to_road_ratio", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue, 10), layer.name="Cycleway to road ratio (%)", hide=TRUE) +
  mapview(grid_mobility_infrastructure |> filter(cycling_quality_ratio <= quantile(cycling_quality_ratio, 1, na.rm=TRUE)), zcol="cycling_quality_ratio", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue, 10), layer.name="Cycling quality ratio (%)", hide=TRUE)

# 6.4 PT stop coverage
bus_stops_isochones_all = impt_read("/mobility/isochrones_buffers_bus_5min.gpkg")
iso_subway_dissolved = impt_read("/mobility/isochrones_subwaylightrail_10min.gpkg")
iso_train_ferry_dissolved = impt_read("/mobility/isochrones_trainferry_15min.gpkg")

mapview(municipios, alpha.regions=0, color="#000000", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
mapview(bus_stops_isochones_all, homebutton = FALSE, layer.control.show = FALSE, col.regions="#a1dab4", layer.name="Bus stop isochrones") + 
  mapview(iso_subway_dissolved, homebutton = FALSE, layer.control.show = FALSE, col.regions="#41b6c4", layer.name="Subway and light rail isochrones") + 
  mapview(iso_train_ferry_dissolved, homebutton = FALSE, layer.control.show = FALSE, col.regions="#2c7fb8", layer.name="Train and ferry isochrones")

grid_stop_coverage <- impt_read("/mobility/grid_pop_stops_coverage.csv") |>
  left_join(grid, by=c("id" = "id")) |>
  st_as_sf() |>
  # pct_pt_bus, pct_pt_all, pct_pt_trainferry, pct_pt_metrolr
  mutate(pct_pt_bus=pct_pt_bus*100, pct_pt_all=pct_pt_all*100, pct_pt_trainferry=pct_pt_trainferry*100, pct_pt_metrolr=pct_pt_metrolr*100)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_stop_coverage, zcol="pct_pt_bus", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue,10), layer.name="Bus stop coverage (%)") +
  mapview(grid_stop_coverage, zcol="pct_pt_all", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue,10), layer.name="PT stop coverage (%)", hide=TRUE) +
  mapview(grid_stop_coverage, zcol="pct_pt_trainferry", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue,10), layer.name="Train & ferry stop coverage (%)", hide=TRUE) +
  mapview(grid_stop_coverage, zcol="pct_pt_metrolr", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue,10), layer.name="Subway & light rail stop coverage (%)", hide=TRUE)

# 6.5 PT headways and waiting times
grid_transit <- impt_read("/mobility_transit/grid_headways.csv") |>
  mutate(frequency_ratio_peak_night = total_frequency_night / total_frequency_peak * 100) |>
  left_join(grid, by=c("id" = "id")) |>
  st_as_sf()
summary(grid_transit$weighted_waiting_time_peak)
summary(grid_transit$frequency_ratio_peak_night)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_transit, zcol="weighted_waiting_time_peak", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToRed,10), layer.name="Weighted waiting time on peak (min)") +
  mapview(grid_transit, zcol="frequency_ratio_peak_night", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(RedToWhite,10), layer.name="Frequency ratio peak/night (%)", hide=TRUE)


# 6.6 Shared mobility
grid_shared_mobility <- impt_read("/mobility/grid_shared_mobility.csv") |>
  left_join(grid, by=c("id" = "id")) |>
  st_as_sf()
summary(grid_shared_mobility$shared_mobility_points)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_shared_mobility, zcol="shared_mobility_points", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis,10), layer.name="Shared mobility points")



## 7. Accessibility --------------------------------------------------------

# 7.1 Cumulative accessibility
grid_accessibility <- impt_read("/accessibility/r8/accessibility_grid.csv") |>
  left_join(grid, by=c("id" = "id")) |>
  st_as_sf()
summary(grid_accessibility$access_health_walk_15min)
summary(grid_accessibility$access_health_bike_15min)
summary(grid_accessibility$access_health_car_30min)
summary(grid_accessibility$access_health_transit_2t_30min)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_accessibility |> filter(!is.na(access_health_walk_15min)) |> select(access_health_walk_15min), zcol="access_health_walk_15min", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue,3), layer.name="Health accessibility for walk (15 min)", hide=TRUE) +
  mapview(grid_accessibility |> filter(!is.na(access_health_bike_15min)) |> select(access_health_bike_15min), zcol="access_health_bike_15min", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue,10), layer.name="Health accessibility by bike (15 min)", hide=TRUE) +
  mapview(grid_accessibility |> filter(!is.na(access_health_car_30min)) |> select(access_health_car_30min), zcol="access_health_car_30min", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue,10), layer.name="Health accessibility by car (30 min)", hide=TRUE)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_accessibility |> filter(!is.na(access_health_transit_2t_30min)), zcol="access_health_transit_2t_30min", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToBlue,10), layer.name="Health accessibility by PT (2 transfers, 30 min)", hide=TRUE)


grid_mobility_costs <- impt_read("/mobility_costs/r8/mobility_grid.csv") |>
  left_join(grid, by=c("id" = "id")) |>
  st_as_sf()
names(grid_mobility_costs)
summary(grid_mobility_costs$mobility_cost_groceries_walk_n1)
summary(grid_mobility_costs$mobility_cost_groceries_walk_n2)
summary(grid_mobility_costs$mobility_cost_groceries_walk_n3)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_mobility_costs |> select(mobility_cost_groceries_walk_n1) |> filter(!is.na(mobility_cost_groceries_walk_n1)), zcol="mobility_cost_groceries_walk_n1", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(GreenToRed,10), layer.name="Travel time to nearest (1) grocery for walk", hide=TRUE) +
  mapview(grid_mobility_costs |> select(mobility_cost_groceries_walk_n2) |>  filter(!is.na(mobility_cost_groceries_walk_n2)), zcol="mobility_cost_groceries_walk_n2", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(GreenToRed,10), layer.name="Travel time to nearest (2) groceries for walk", hide=TRUE) +
  mapview(grid_mobility_costs |> select(mobility_cost_groceries_walk_n3) |> filter(!is.na(mobility_cost_groceries_walk_n3)), zcol="mobility_cost_groceries_walk_n3", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(GreenToRed,10), layer.name="Travel time to nearest (3) groceries for walk", hide=TRUE)


## 8  Affordability --------------------------------------------------------

grid_affordability_car <- impt_read("/mobility_money_costs/grid_commuting_money_car.csv") |> 
  left_join(grid, by=c("id_grid_origin" = "id")) |>
  st_as_sf() |>
  mutate(total_money = round(total_money, digits=2))
grid_affordability_pt_single_fare <- impt_read("/mobility_money_costs/grid_commuting_money_pt_single_fare.csv") |>
  left_join(grid, by=c("id_grid_origin" = "id")) |>
  st_as_sf() |>
  mutate(total_money = round(total_money, digits=2)) |>
  filter(!is.na(total_money))
grid_affordability_pt_pass <- impt_read("/mobility_money_costs/grid_commuting_money_pt_pass_fare.csv") |>
  left_join(grid, by=c("id_grid_origin" = "id")) |>
  st_as_sf() |>
  mutate(total_money = round(total_money, digits=2)) |>
  filter(!is.na(total_money))
summary(grid_affordability_car$total_money)
summary(grid_affordability_pt_single_fare$total_money)
summary(grid_affordability_pt_pass$total_money)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_affordability_car, zcol="total_money", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis,10), layer.name="Commuting cost by car (€)") +
  mapview(grid_affordability_pt_single_fare, zcol="total_money", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis,10), layer.name="Commuting cost by PT (Single Fare, €)", hide=TRUE) +
  mapview(grid_affordability_pt_pass, zcol="total_money", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis,10), layer.name="Commuting cost by PT (Monthly Pass, €)", hide=TRUE)

# 8.3 Composite affordability indicators
grid_affordability_composite <- impt_read("/affordability/affordability_grid_composite.csv") |>
  left_join(grid, by=c("grid_id" = "id")) |>
  st_as_sf() |>
  # transp_inc_comp_nav, h_transp_inc_comp_nav
  mutate(transp_inc_comp_nav = round(transp_inc_comp_nav*100, digits=2), h_transp_inc_comp_nav = round(h_transp_inc_comp_nav*100, digits=2))
names(grid_affordability_composite)
summary(grid_affordability_composite$transp_inc_comp_nav)
summary(grid_affordability_composite$h_transp_inc_comp_nav)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_affordability_composite |> filter(!is.na(transp_inc_comp_nav)), zcol="transp_inc_comp_nav", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis,10), layer.name="Transport burden (%)") +
  mapview(grid_affordability_composite|> filter(!is.na(h_transp_inc_comp_nav)), zcol="h_transp_inc_comp_nav", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis,10), layer.name="Transport + housing burden (%)", hide=TRUE)

# 8.4 Income, Gini Coefficient and Palma Ratio
grid_census_income <- impt_read("/landuse/grid_income_housing_gini.csv")  |>
  left_join(grid, by=c("grid_id" = "id")) |>
  st_as_sf()
names(grid_census_income)
summary(grid_census_income$gini_coef)
summary(grid_census_income$palma_ratio)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_census_income |> filter(!is.na(gini_coef)), zcol="gini_coef", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis,10), layer.name="Gini coefficient") +
  mapview(grid_census_income |> filter(!is.na(palma_ratio)), zcol="palma_ratio", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis,10), layer.name="Palma ratio", hide=TRUE)

summary(grid_census_income$housing_costs)

mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_census_income |> filter(!is.na(housing_costs)), zcol="housing_costs", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(Viridis,10), layer.name="Housing costs (€)")



## 9  Safety ---------------------------------------------------------------

grid_safety_inner <- impt_read("/safety/accidents_by_grid_5years_dentrolocalidades.csv") |>
  left_join(grid, by=c("grid_id" = "id")) |>
  st_as_sf() |>
  mutate(indice_gravidade = indice_gravidade*100)
names(grid_safety_inner)
summary(grid_safety_inner$acidentes_per_1000res)
summary(grid_safety_inner$indice_gravidade)

q = quantile(grid_safety_inner$acidentes_per_1000res, 0.995, na.rm=TRUE)
q
mapview(grid, alpha.regions=0, color="#808080", homebutton = FALSE, layer.control.show = FALSE, legend=FALSE) +
  mapview(grid_safety_inner |> filter(!is.na(acidentes_per_1000res) & acidentes_per_1000res<q), zcol="acidentes_per_1000res", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToRed,10), layer.name="Accidents per 1000 residents") +
  mapview(grid_safety_inner |> filter(!is.na(indice_gravidade)), zcol="indice_gravidade", homebutton = FALSE, layer.control.show = FALSE, col.regions=select_equidistant(WhiteToRed,10), layer.name="Severity index (%)", hide=TRUE)



## 10  Index Computation — IMPT Calculator ---------------------------------

names(freguesias_aggregated)
summary(freguesias_aggregated$IMPT_score_pca_geom_pass)
summary(freguesias_aggregated$IMPT_score_pca_avg_pass)
summary(freguesias_aggregated$IMPT_entropy_pca_pass)

mapview(
  freguesias_aggregated |> select(IMPT_score_pca_geom_pass) |> arrange(IMPT_score_pca_geom_pass), 
  homebutton = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  zcol="IMPT_score_pca_geom_pass",
  col.regions = select_equidistant(GreenToRed, 10),
  layer.name="IMPT with Geometric Mean"
) +
mapview(
  freguesias_aggregated |> select(IMPT_score_pca_avg_pass),
  homebutton = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  zcol="IMPT_score_pca_avg_pass",
  col.regions = select_equidistant(GreenToRed, 10),
  layer.name="IMPT with Arithmetic mean",
  hide = TRUE
) +
mapview(
  freguesias_aggregated |> select(IMPT_entropy_pca_pass),
  homebutton = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  zcol="IMPT_entropy_pca_pass",
  col.regions = select_equidistant(GreenToRed, 10),
  layer.name="IMPT with Entropy",
  hide = TRUE
)

# Draw IMPT with Geometric Mean for grid and municipality
mapview(
  grid_aggregated |> select(IMPT_score_pca_geom_pass) |> arrange(IMPT_score_pca_geom_pass), 
  homebutton = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  zcol="IMPT_score_pca_geom_pass",
  col.regions = select_equidistant(GreenToRed, 10),
  layer.name="IMPT with Geometric Mean"
)
mapview(
  municipios_aggregated |> select(IMPT_score_pca_geom_pass) |> arrange(IMPT_score_pca_geom_pass), 
  homebutton = FALSE, layer.control.show = FALSE, # Hide elements for printscreen
  zcol="IMPT_score_pca_geom_pass",
  col.regions = select_equidistant(GreenToRed, 10),
  layer.name="IMPT with Geometric Mean"
)
