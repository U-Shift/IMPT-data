# aim: retrieve the vehicle ownership rate (car, bicycle, car_other, none), per family, per freguesia
# retrieve the car_driving licence rate per freguesia

library(tidyverse)

# load imob data
IMOB = readRDS("data/IMOB_trips.Rds")
names(IMOB)

# we don't have the freguesia of where people live, only the city.
# we will rely on trip purpose "Return Home" and city.

IMOB = IMOB |>
  mutate(trip_purpose = case_when(
    
    # --- Work ---
    D0500_Dsg == "Ir para o trabalho" ~ "Commute to Work",
    D0500_Dsg == "Tratar de assuntos profissionais" ~ "Work (Other)",
    
    # --- Education ---
    D0500_Dsg == "Ir para a escola ou atividades escolares" ~ "Education",
    
    # --- Return home ---
    D0500_Dsg == "Regressar a casa" ~ "Return Home",
    
    # --- Escort trips ---
    D0500_Dsg == "Levar/buscar/acompanhar familiares ou amigos (crianças à escola, etc)" ~ "Escort",
    
    # --- Leisure ---
    D0500_Dsg %in% c(
      "Assistir a eventos desportivos ou culturais (cinema, teatro, concerto, futebol, etc.)",
      "Ir a restaurante, café, bar, discoteca, etc.",
      "Outras atividades de lazer, entretenimento ou turismo",
      "Praticar atividades ao ar livre (desporto ou lazer) ou em ginásio ou pavilhão",
      "Fazer percurso pedonal (início e fim no mesmo local), jogging, passear o cão, etc. (com pelo menos 200 metros)",
      "Realizar atividade em grupo ou em contexto coletivo (em associações, comícios, igrejas, voluntariado, ...)",
      "Visitar familiares ou amigos"
    ) ~ "Leisure",
    
    # --- Utilitarian / personal maintenance ---
    D0500_Dsg %in% c(
      "Fazer compras (supermercado, mercearia, utilidades, etc)",
      "Ir a consulta, tratamentos, exames médicos e similares",
      "Tratar de assuntos pessoais (ir ao banco, lavandaria, cabeleireiro, levar ou buscar coisas pessoais, etc)"
    ) ~ "Utilitarian",
    
    # --- Other ---
    D0500_Dsg == "Outra atividade" ~ "Other",
    
    TRUE ~ "Other"
  ))


# Instead of first(Destination_dicofre16), use the most frequent return-home destination per household (more robust):

# home_location <- IMOB |>
#   filter(trip_purpose == "Return Home") |>
#   count(Id_aloj_1, Destination_dicofre16, wt = PESOFIN) |>
#   group_by(Id_aloj_1) |>
#   slice_max(n, n = 1, with_ties = FALSE) |>
#   ungroup() |>
#   left_join(
#     IMOB |>
#       select(Id_aloj_1, PESOFIN, N_Automoveis, N_VMercadorias, N_VOutros,
#              N_Motociclos, N_Bicicletas, NaoDispoeVeiculos) |>
#       distinct(),
#     by = "Id_aloj_1"
#   ) |>
#   rename(
#     dicofre_home = Destination_dicofre16,
#     hh_weight = PESOFIN,
#     cars = N_Automoveis,
#     cars_merc = N_VMercadorias,
#     cars_other = N_VOutros,
#     motorcycles = N_Motociclos,
#     bicycles = N_Bicicletas,
#     no_veh = NaoDispoeVeiculos
#   )
# 
# 
# n_distinct(IMOB$Id_aloj_1) #22098
# n_distinct(home_location$Id_aloj_1) #21005

# get the 1093 missing "return home" based on the origin of the first trip

home_from_return <- IMOB |>
  filter(trip_purpose == "Return Home") |>
  count(Id_aloj_1, Destination_dicofre16, wt = PESOFIN) |>
  group_by(Id_aloj_1) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    Id_aloj_1,
    dicofre_home = Destination_dicofre16
  )

hh_without_home <- IMOB |>
  distinct(Id_aloj_1) |>
  anti_join(home_from_return, by = "Id_aloj_1")

home_from_first_trip <- IMOB |>
  semi_join(hh_without_home, by = "Id_aloj_1") |>
  arrange(Id_aloj_1, N_Desloc) |>   # use trip order
  group_by(Id_aloj_1) |>
  slice(1) |> # use only the fisrst trip
  ungroup() |>
  transmute(
    Id_aloj_1,
    dicofre_home = Origin_dicofre16
  )

home_dicofre <- bind_rows(home_from_return, home_from_first_trip)


n_distinct(IMOB$Id_aloj_1) # 22098
n_distinct(home_dicofre$Id_aloj_1) #22098 !!!

# not do the same but all sample
home_location <- home_dicofre |>
  left_join(
    IMOB |>
      select(Id_aloj_1, PESOFIN, DTCC_aloj, Zona_aloj,
             N_Automoveis, N_VMercadorias, N_VOutros,
             N_Motociclos, N_Bicicletas, NaoDispoeVeiculos) |>
      distinct(),
    by = "Id_aloj_1"
  ) |>
  rename(
    hh_weight = PESOFIN,
    cars = N_Automoveis,
    cars_merc = N_VMercadorias,
    cars_other = N_VOutros,
    motorcycles = N_Motociclos,
    bicycles = N_Bicicletas,
    no_veh = NaoDispoeVeiculos
  )

# Check with DTCC that we know
home_location_check = home_location |> select(Id_aloj_1, dicofre_home, DTCC_aloj) |> 
  mutate(dicofre_home_4 = substr(dicofre_home, 1, 4)) |> 
  mutate(verification = dicofre_home_4 == DTCC_aloj)

table(home_location_check$verification)
# FALSE  TRUE 
# 184   21914 

# I also tried with the most frequent first trip from the household, but the results are the same.
# the error is lower than 1% (0.8%). we will proceed.

vehicles_hh = home_location |> select(-DTCC_aloj, -Zona_aloj)

# # problem with "more than...". We will use the higher number +1
# table(vehicles_hh$cars)
# table(vehicles_hh$cars_merc)
# table(vehicles_hh$cars_other)
# table(vehicles_hh$motorcycles) #>=4
# table(vehicles_hh$bicycles) #>=7
# table(vehicles_hh$no_veh)

vehicles_hh <- vehicles_hh |> 
  mutate(
    motorcycles = ifelse(motorcycles == ">=4", 4, as.numeric(motorcycles)),
    bicycles    = ifelse(bicycles == ">=7", 7, as.numeric(bicycles))
  ) |> 
  mutate(across(c(cars, cars_merc, cars_other, motorcycles, bicycles), ~replace_na(., 0)))

# table(is.na(vehicles_hh$cars))
# table(is.na(vehicles_hh$cars_merc))
# table(is.na(vehicles_hh$cars_other))
# table(is.na(vehicles_hh$motorcycles)) 
# table(is.na(vehicles_hh$bicycles))
# table(is.na(vehicles_hh$no_veh))

# solve the total vehicles and no_veh
vehicles_hh <- vehicles_hh  |> 
  mutate(
    # If no vehicles at all, set no_veh = 1
    total_vehicles = cars + cars_merc + cars_other + motorcycles + bicycles,
    total_motor_vehicles = cars + cars_merc + cars_other + motorcycles ) |> 
  select(-no_veh)

# 
# vehicles_by_dicofre <- vehicles_hh |> 
#   group_by(dicofre_home) |> 
#   summarise(
#     hh_sample = n(), # number of households
#     total_weight = sum(hh_weight, na.rm = TRUE),
#     avg_cars  = weighted.mean(cars, hh_weight, na.rm = TRUE),
#     avg_cars_merc = weighted.mean(cars_merc, hh_weight, na.rm = TRUE),
#     avg_cars_other = weighted.mean(cars_other, hh_weight, na.rm = TRUE),
#     avg_motorcycles = weighted.mean(motorcycles, hh_weight, na.rm = TRUE),
#     avg_bicycles = weighted.mean(bicycles, hh_weight, na.rm = TRUE),
#     total_vehicles_per_hh = weighted.mean(total_vehicles, hh_weight, na.rm = TRUE),
#     total_motor_vehicles_per_hh = weighted.mean(total_motor_vehicles, hh_weight, na.rm = TRUE),
#     pct_no_vehicle = sum(hh_weight[total_vehicles == 0], na.rm = TRUE) / sum(hh_weight, na.rm = TRUE) * 100,
#     .groups = "drop"
#   )

# now replace the dicofres new

vehicles_by_dicofre_new <- vehicles_hh |>
  # join with conversion table on old dicofre
  left_join(conversion_dicofre_weight, by = c("dicofre_home" = "dtmnfr16")) |>
  # scale only the household weight according to area weight
  mutate(hh_weight_new = hh_weight * weight) |>
  # rename the new dicofre
  rename(dicofre_home_new = dtmnfr24) |>
  # group by new dicofre
  group_by(dicofre_home_new) |>
  summarise(
    n_households = n(),                      # number of original households contributing
    hh_weight    = sum(hh_weight_new),       # sum of weighted households
    avg_cars         = weighted.mean(cars, hh_weight_new, na.rm = TRUE),
    avg_cars_merc    = weighted.mean(cars_merc, hh_weight_new, na.rm = TRUE),
    avg_cars_other   = weighted.mean(cars_other, hh_weight_new, na.rm = TRUE),
    avg_motorcycles  = weighted.mean(motorcycles, hh_weight_new, na.rm = TRUE),
    avg_bicycles     = weighted.mean(bicycles, hh_weight_new, na.rm = TRUE),
    total_vehicles_per_hh       = weighted.mean(total_vehicles, hh_weight_new, na.rm = TRUE),
    total_motor_vehicles_per_hh = weighted.mean(total_motor_vehicles, hh_weight_new, na.rm = TRUE),
    pct_hh_no_vehicle = sum(hh_weight_new[total_vehicles == 0], na.rm = TRUE) / sum(hh_weight_new, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |> 
  rename(dicofre = dicofre_home_new)

# quick check
summary(vehicles_by_dicofre$avg_cars)
summary(vehicles_by_dicofre_new$avg_cars)
summary(vehicles_by_dicofre$pct_no_vehicle)
summary(vehicles_by_dicofre_new$pct_hh_no_vehicle)


vehicles_by_dicofre_new_geo = vehicles_by_dicofre_new |>
  left_join(freguesias, by = c("dicofre" = "dtmnfr")) |> 
  st_as_sf()
mapview::mapview(vehicles_by_dicofre_new_geo, zcol="avg_cars")
mapview::mapview(vehicles_by_dicofre_new_geo, zcol="avg_bicycles")


# export
saveRDS(vehicles_by_dicofre_new, "data/imob_vehicles_freg.Rds")
write.csv(vehicles_by_dicofre_new, "data/imob_vehicles_freg.csv", row.names = FALSE)
