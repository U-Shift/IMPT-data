# Census modal share
# Purpose     Determine the modal share for each scale
# Scale       hex, parish, municipality
# Issue       -

# Read CSV
database <- impt_read("/census2021/ine_indicador_0011704.csv", csv_sep = ";")
# names(database)
# [1] "Local.de.residência.à.data.dos.Censos..2021...NUTS...2013."
# [2] "S7A2021.2021.T.HM.T.Total.T.Total.T.Total"
# [3] "S7A2021.2021.T.HM.T.Total.01.A.pé.T.Total"
# [4] "S7A2021.2021.T.HM.T.Total.02.Automovel.ligeiro...como.condutor.T.Total"
# [5] "S7A2021.2021.T.HM.T.Total.03.Automovel.ligeiro...como.passageiro.T.Total"
# [6] "S7A2021.2021.T.HM.T.Total.04.Autocarro.T.Total"
# [7] "S7A2021.2021.T.HM.T.Total.05.Transporte.coletivo.da.empresa.ou.da.escola.T.Total"
# [8] "S7A2021.2021.T.HM.T.Total.06.Metropolitano.T.Total"
# [9] "S7A2021.2021.T.HM.T.Total.07.Comboio.T.Total"
# [10] "S7A2021.2021.T.HM.T.Total.08.Motociclo.T.Total"
# [11] "S7A2021.2021.T.HM.T.Total.09.Bicicleta.T.Total"
# [12] "S7A2021.2021.T.HM.T.Total.10.Barco.T.Total"
# [13] "S7A2021.2021.T.HM.T.Total.11.Outro.T.Total"
# Rename first column to str_id
names(database)
names(database)[1] <- "str_id"
names(database)[2] <- "total"
names(database)[3] <- "walk"
names(database)[4] <- "car_driver"
names(database)[5] <- "car_passenger"
names(database)[6] <- "bus"
names(database)[7] <- "company_school_transport"
names(database)[8] <- "metro"
names(database)[9] <- "train"
names(database)[10] <- "motorcycle"
names(database)[11] <- "bike"
names(database)[12] <- "boat"
names(database)[13] <- "other"


database <- database |>
  mutate(dtmnfr16 = sub(":.*", "", str_id)) |>
  filter(!dtmnfr16 %in% c("17", "170")) |>
  mutate(
    pt = bus + company_school_transport + metro + train + boat,
    private_vehicle = car_driver + car_passenger + motorcycle,
    active = walk + bike
  ) |>
  select(dtmnfr16, total, pt, private_vehicle, active, walk, bike)
nrow(database)

# Modal share computation, with dicofre convertion
all_dicofre_conversion_weight <- readRDS("useful_data/dicofre_16_24_conversion_full_with_weights.Rds")
all_dicofre_conversion_weight

database_converted <- all_dicofre_conversion_weight |>
  left_join(database, by = c("dtmnfr16")) |>
  mutate(
    total_converted = round(total * weight, digits = 2),
    pt_converted = round(pt * weight, digits = 2),
    private_vehicle_converted = round(private_vehicle * weight, digits = 2),
    active_converted = round(active * weight, digits = 2),
    walk_converted = round(walk * weight, digits = 2),
    bike_converted = round(bike * weight, digits = 2)
  )

assertthat::assert_that(nrow(database_converted) == nrow(all_dicofre_conversion_weight))
assertthat::assert_that(sum(database_converted$total_converted) == sum(database$total))
assertthat::assert_that(sum(database_converted$pt_converted) == sum(database$pt))
assertthat::assert_that(sum(database_converted$private_vehicle_converted) == sum(database$private_vehicle))
assertthat::assert_that(sum(database_converted$active_converted) == sum(database$active))
assertthat::assert_that(sum(database_converted$walk_converted) == sum(database$walk))
assertthat::assert_that(sum(database_converted$bike_converted) == sum(database$bike))

database_final <- database_converted |>
  select(dtmnfr24, total_converted, pt_converted, private_vehicle_converted, active_converted, walk_converted, bike_converted) |>
  rename(
    dtmnfr = dtmnfr24,
    total = total_converted,
    pt = pt_converted,
    private_vehicle = private_vehicle_converted,
    active = active_converted,
    walk = walk_converted,
    bike = bike_converted
  ) |>
  mutate(
    pt_share = round(pt / total, digits = 2),
    private_vehicle_share = round(private_vehicle / total, digits = 2),
    active_share = round(active / total, digits = 2),
    walk_share = round(walk / total, digits = 2),
    bike_share = round(bike / total, digits = 2)
  )
View(database_final)

write.csv(database_final, "useful_data/census_modal_share.csv", row.names = FALSE)

# Compute for municipality
mun_parish <- read.csv("useful_data/freguesias_nuts.csv")

database_municipality <- database_converted |>
  select(dtmnfr24, total_converted, pt_converted, private_vehicle_converted, active_converted, walk_converted, bike_converted) |>
  rename(
    total = total_converted,
    pt = pt_converted,
    private_vehicle = private_vehicle_converted,
    active = active_converted,
    walk = walk_converted,
    bike = bike_converted
  ) |>
  left_join(mun_parish |> select(freg_id, mun_id) |> mutate(freg_id = as.character(freg_id)), by = c("dtmnfr24" = "freg_id")) |>
  group_by(mun_id) |>
  summarise(
    total = sum(total),
    pt = sum(pt),
    private_vehicle = sum(private_vehicle),
    active = sum(active),
    walk = sum(walk),
    bike = sum(bike)
  ) |>
  mutate(
    pt_share = round(pt / total, digits = 2),
    private_vehicle_share = round(private_vehicle / total, digits = 2),
    active_share = round(active / total, digits = 2),
    walk_share = round(walk / total, digits = 2),
    bike_share = round(bike / total, digits = 2)
  )

# mun_id = read.csv("useful_data/mun_nuts.csv")
# mapview::mapview(municipios |> left_join(mun_id, by=c("municipio"="name")) |> left_join(database_municipality, by = c("mun_id" = "id")), zcol="pt_share")
# mapview::mapview(municipios |> left_join(mun_id, by=c("municipio"="name")) |> left_join(database_municipality, by = c("mun_id" = "id")), zcol="private_vehicle_share")
# mapview::mapview(municipios |> left_join(mun_id, by=c("municipio"="name")) |> left_join(database_municipality, by = c("mun_id" = "id")), zcol="active_share")

database_grid <- read.csv("useful_data/grid_nuts.csv") |>
  select(grid_id, freg_id) |>
  rename(id = grid_id, dicofre = freg_id) |>
  mutate(dtmnfr = as.character(dicofre)) |>
  filter(!is.na(dtmnfr)) |>
  left_join(database_final, by = "dtmnfr")
# mapview::mapview(grid |> left_join(database_grid), zcol="pt_share")
# mapview::mapview(grid |> left_join(database_grid), zcol="active_share")


impt_write(database_final, "census2021/census_modal_share_parish.csv")
impt_write(database_municipality, "census2021/census_modal_share_municipality.csv")
impt_write(database_grid, "census2021/census_modal_share_grid.csv")
