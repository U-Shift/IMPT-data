# =============================================================================
# 04_affordability.R
# Pre-calculates raw and weighted affordability metrics by freguesia.
# Exports to /data/IMPT/affordability/affordability_freguesia_composite.csv so that
# 05_IMPTcalculator.R can read it and proceed cleanly.
# =============================================================================

# ── 0. Libraries ──────────────────────────────────────────────────────────────
library(dplyr)
library(readr)
library(stringr)

# 0. Load external data ---------------------------------------------------
municipios_id <- read_csv("useful_data/municipios_id.csv")
grid_freg_mun <- read_csv("useful_data/grid_nuts.csv")

# ── 1. Data Definitions & Imports ─────────────────────────────────────────────

## definitions
trips_commuting_year <- 2 * 250 # 2 trips per day, 250 working days per year (weekends excluded)

##### car occupancy rate and mobile population, by freguesia ####
# occupancy rate for car by municipality (IMOB_2017_AML.xlsx, sheet "Quadro IV.7 >> Taxa de ocupação dos automóveis por município de residência")
occ_rate_car_freg <- readr::read_delim(
  IMPT_URL("/trips/imob_taxa_occ_auto.csv"),
  # delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) |>
  left_join(municipios_id, by = c("Municipio" = "municipio")) |>
  # mutate(mun_id = as.character(mun_id)) |>
  select(-Municipio) |>
  left_join(grid_freg_mun |> select(-grid_id), by = "mun_id") |>
  distinct() |>
  mutate(
    taxa_ocup_auto = taxa_ocup_auto,
    freg_id = as.integer(freg_id)
  )

# mobile population by municipality (IMOB_2017_AML.xlsx, sheet "Quadro II.1 >> População móvel por município de residência")
pop_movel_freg <- readr::read_delim(
  IMPT_URL("/trips/imob_populacao_movel.csv"),
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) |>
  left_join(municipios_id, by = c("Municipio" = "municipio")) |>
  # mutate(mun_id = as.character(mun_id)) |>
  select(-Municipio) |>
  left_join(grid_freg_mun |> select(-grid_id), by = "mun_id") |>
  distinct() |>
  mutate(
    pop_movel = pop_movel / 1000, # só tinha 1 casa decimal
    freg_id = as.integer(freg_id)
  )


#### household size
households_size_freg <- read_csv(IMPT_URL("/landuse/landuse_freguesias.csv")) |>
  select(freg_id, population, households) |>
  mutate(pp_hh_avg = population / households)

## import previous computed costs
freguesia_affordability_mob <- freguesia_affordability_car |>
  rename(dtmnfr = Origin_dicofre24) |>
  select(dtmnfr, total_money) |>
  rename(affordability_car_total_money = total_money) |>
  left_join(freguesia_affordability_pt_pass |> rename(dtmnfr = Origin_dicofre24) |> select(dtmnfr, total_money) |> rename(affordability_transit_pass_total_money = total_money), by = "dtmnfr") |>
  left_join(freguesia_affordability_pt_single_fare |> rename(dtmnfr = Origin_dicofre24) |> select(dtmnfr, total_money) |> rename(affordability_transit_single_fare_total_money = total_money), by = "dtmnfr")

freguesia_income <- read_csv(IMPT_URL("/landuse/freguesias_income_housing_gini.csv")) |>
  select(-gini_coef) |>
  rename(dtmnfr = freg_id) |>
  mutate(housing_costs_year = housing_costs * 12) # yearly costs

freguesia_affordability <- freguesia_affordability_mob |>
  left_join(freguesia_income, by = "dtmnfr") |>
  left_join(occ_rate_car_freg |> select(freg_id, taxa_ocup_auto), by = c("dtmnfr" = "freg_id")) |>
  left_join(pop_movel_freg |> select(freg_id, pop_movel), by = c("dtmnfr" = "freg_id")) |>
  left_join(households_size_freg |> select(freg_id, pp_hh_avg), by = c("dtmnfr" = "freg_id")) |>
  mutate(
    affordability_car_total_money = (affordability_car_total_money / taxa_ocup_auto) * pp_hh_avg * pop_movel, # adjust car costs by occupancy and household size and mobile population)
    affordability_transit_pass_total_money = affordability_transit_pass_total_money * pp_hh_avg * pop_movel, # adjust transit costs by household size and mobile population
    affordability_transit_single_fare_total_money = affordability_transit_single_fare_total_money * pp_hh_avg * pop_movel # adjust transit costs by household size and mobile population
  )

# Census modal share (used to compute modal-share-weighted affordability)
census_modal_share <- read_csv(IMPT_URL("/census2021/census_modal_share_parish.csv")) |>
  select(dtmnfr, total, pt, private_vehicle, active, pt_share, private_vehicle_share, active_share)


# NAVEGANTE monthly pass scenario
Affordability_navegante <- freguesia_affordability |>
  mutate(
    h_transp_inc_car = (housing_costs_year + affordability_car_total_money * trips_commuting_year) / income_hh,
    h_transp_inc_pt  = (housing_costs_year + affordability_transit_pass_total_money * trips_commuting_year) / income_hh,
    transp_inc_car   = (affordability_car_total_money * trips_commuting_year) / income_hh,
    transp_inc_pt    = (affordability_transit_pass_total_money * trips_commuting_year) / income_hh
  ) |>
  select(dtmnfr, h_transp_inc_car, h_transp_inc_pt, transp_inc_car, transp_inc_pt)

# Impute Canha (dtmnfr == "150701") with the mean of its neighbours in the same municipality
mean_viz_canha <- Affordability_navegante |>
  filter(str_starts(dtmnfr, "1507"), dtmnfr != "150701") |>
  summarise(
    mean_h_transp_inc_pt = mean(h_transp_inc_pt, na.rm = TRUE),
    mean_transp_inc_pt   = mean(transp_inc_pt, na.rm = TRUE)
  )

Affordability_navegante <- Affordability_navegante |> # take care of NA at Canha
  mutate(
    h_transp_inc_pt = case_when(dtmnfr == "150701" ~ mean_viz_canha$mean_h_transp_inc_pt, TRUE ~ h_transp_inc_pt),
    transp_inc_pt   = case_when(dtmnfr == "150701" ~ mean_viz_canha$mean_transp_inc_pt, TRUE ~ transp_inc_pt)
  ) |>
  left_join(census_modal_share, by = "dtmnfr") |>
  mutate(
    h_transp_inc_comp = (h_transp_inc_car * private_vehicle_share + h_transp_inc_pt * pt_share) / (private_vehicle_share + pt_share + active_share),
    transp_inc_comp   = (transp_inc_car * private_vehicle_share + transp_inc_pt * pt_share) / (private_vehicle_share + pt_share + active_share)
  ) |>
  select(dtmnfr, h_transp_inc_car, h_transp_inc_pt, transp_inc_car, transp_inc_pt, h_transp_inc_comp, transp_inc_comp)

# SINGLE FARE scenario
Affordability_singlefare <- freguesia_affordability |>
  mutate(
    h_transp_inc_car = (housing_costs_year + affordability_car_total_money * trips_commuting_year) / income_hh, # with housing costs
    h_transp_inc_pt  = (housing_costs_year + affordability_transit_single_fare_total_money * trips_commuting_year) / income_hh,
    transp_inc_car   = (affordability_car_total_money * trips_commuting_year) / income_hh, # without housing costs
    transp_inc_pt    = (affordability_transit_single_fare_total_money * trips_commuting_year) / income_hh
  ) |>
  select(dtmnfr, h_transp_inc_car, h_transp_inc_pt, transp_inc_car, transp_inc_pt)

mean_viz_canha2 <- Affordability_singlefare |> # take care of NA at Canha
  filter(str_starts(dtmnfr, "1507"), dtmnfr != "150701") |>
  summarise(
    mean_h_transp_inc_pt = mean(h_transp_inc_pt, na.rm = TRUE),
    mean_transp_inc_pt   = mean(transp_inc_pt, na.rm = TRUE)
  )

Affordability_singlefare <- Affordability_singlefare |>
  mutate(
    h_transp_inc_pt = case_when(dtmnfr == "150701" ~ mean_viz_canha2$mean_h_transp_inc_pt, TRUE ~ h_transp_inc_pt),
    transp_inc_pt   = case_when(dtmnfr == "150701" ~ mean_viz_canha2$mean_transp_inc_pt, TRUE ~ transp_inc_pt)
  ) |>
  left_join(census_modal_share, by = "dtmnfr") |>
  mutate( # accounting for modal share, with and without housing
    h_transp_inc_comp = (h_transp_inc_car * private_vehicle_share + h_transp_inc_pt * pt_share) / (private_vehicle_share + pt_share + active_share),
    transp_inc_comp   = (transp_inc_car * private_vehicle_share + transp_inc_pt * pt_share) / (private_vehicle_share + pt_share + active_share)
  ) |>
  select(dtmnfr, h_transp_inc_car, h_transp_inc_pt, transp_inc_car, transp_inc_pt, h_transp_inc_comp, transp_inc_comp)



# ── 2. Export RAW Affordability ──────────────────────────────────────────────
# Export a comprehensive table of raw affordability variables before PCA
affordability_freguesia_composite <- freguesia_affordability |>
  select(
    dtmnfr, income_hh, housing_costs_year,
    affordability_car_total_money,
    affordability_transit_pass_total_money,
    affordability_transit_single_fare_total_money
  ) |>
  mutate(
    yearly_cost_car = affordability_car_total_money * trips_commuting_year,
    yearly_cost_pt_navegante = affordability_transit_pass_total_money * trips_commuting_year,
    yearly_cost_pt_singlefare = affordability_transit_single_fare_total_money * trips_commuting_year
  ) |>
  select(dtmnfr, income_hh, housing_costs_year, yearly_cost_car, yearly_cost_pt_navegante, yearly_cost_pt_singlefare) |>
  left_join(
    Affordability_navegante |> select(
      dtmnfr,
      transp_inc_car,
      transp_inc_pt_nav = transp_inc_pt,
      transp_inc_comp_nav = transp_inc_comp,
      h_transp_inc_car,
      h_transp_inc_pt_nav = h_transp_inc_pt,
      h_transp_inc_comp_nav = h_transp_inc_comp
    ),
    by = "dtmnfr"
  ) |>
  left_join(
    Affordability_singlefare |> select(
      dtmnfr,
      transp_inc_pt_sf = transp_inc_pt,
      transp_inc_comp_sf = transp_inc_comp,
      h_transp_inc_pt_sf = h_transp_inc_pt,
      h_transp_inc_comp_sf = h_transp_inc_comp
    ),
    by = "dtmnfr"
  ) |>
  left_join(census_modal_share |> select(dtmnfr, private_vehicle_share, pt_share, active_share), by = "dtmnfr") |>
  mutate(
    yearly_cost_comp_nav = (yearly_cost_car * private_vehicle_share + yearly_cost_pt_navegante * pt_share) / (private_vehicle_share + pt_share + active_share),
    yearly_cost_comp_sf = (yearly_cost_car * private_vehicle_share + yearly_cost_pt_singlefare * pt_share) / (private_vehicle_share + pt_share + active_share)
  ) |>
  select(
    dtmnfr, income_hh, housing_costs_year,
    yearly_cost_car, yearly_cost_pt_navegante, yearly_cost_pt_singlefare, yearly_cost_comp_nav, yearly_cost_comp_sf,
    transp_inc_car, transp_inc_pt_nav, transp_inc_pt_sf, transp_inc_comp_nav, transp_inc_comp_sf,
    h_transp_inc_car, h_transp_inc_pt_nav, h_transp_inc_pt_sf, h_transp_inc_comp_nav, h_transp_inc_comp_sf
  )

write_csv(affordability_freguesia_composite, "/data/IMPT/affordability/affordability_freguesia_composite.csv")


## At Municipio level

# Municipio level ---------------------------------------------------------

# Population lookup: used to compute population-weighted means at higher levels
census24_fregmun_pop <- read_csv("useful_data/census24_fregmun.csv") |>
  select(freg_id, mun_id, population) |>
  mutate(dtmnfr = as.integer(freg_id))

# Grid lookup: assigns each grid cell to a freguesia (generalised assumption)
grid_freg_mun <- read.csv("useful_data/grid_nuts.csv") |>
  select(grid_id, freg_id) |>
  mutate(freg_id = as.integer(freg_id))


# Helper: aggregate from freguesia to a higher spatial unit using pop-weighted mean
aggregate_to_level <- function(df, lookup, by_col) {
  df |>
    left_join(lookup, by = "dtmnfr") |>
    group_by(across(all_of(by_col))) |>
    summarize_at(vars(-dtmnfr, -freg_id, -population), ~ weighted.mean(., w = population, na.rm = TRUE))
}

affordability_municipio_composite <- aggregate_to_level(affordability_freguesia_composite, census24_fregmun_pop, "mun_id")
write_csv(affordability_municipio_composite, "/data/IMPT/affordability/affordability_municipio_composite.csv")



# Grid level --------------------------------------------------------------
grid_freg_mun <- read.csv("useful_data/grid_nuts.csv")

# car occupancy rate by grid
# occupancy rate for car by municipality (IMOB_2017_AML.xlsx, sheet "Quadro IV.7 >> Taxa de ocupação dos automóveis por município de residência")
occ_rate_car_grid <- readr::read_delim(
  IMPT_URL("/trips/imob_taxa_occ_auto.csv"),
  # delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) |>
  left_join(municipios_id, by = c("Municipio" = "municipio")) |>
  # mutate(mun_id = as.character(mun_id)) |>
  select(-Municipio) |>
  left_join(grid_freg_mun, by = "mun_id") |>
  distinct() |>
  mutate(
    taxa_ocup_auto = taxa_ocup_auto,
    grid_id = as.integer(grid_id)
  )
# mobile population by municipality (IMOB_2017_AML.xlsx, sheet "Quadro II.1 >> População móvel por município de residência")
pop_movel_grid <- readr::read_delim(
  IMPT_URL("/trips/imob_populacao_movel.csv"),
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) |>
  left_join(municipios_id, by = c("Municipio" = "municipio")) |>
  # mutate(mun_id = as.character(mun_id)) |>
  select(-Municipio) |>
  left_join(grid_freg_mun |> select(-freg_id), by = "mun_id") |>
  distinct() |>
  mutate(
    pop_movel = pop_movel / 1000, # só tinha 1 casa decimal
    grid_id = as.integer(grid_id)
  )


#### household size
households_size_grid <- read_csv(IMPT_URL("/landuse/grid_with_cos.csv")) |>
  select(grid_id = id, population, households, pp_hh_avg)


## import previous computed costs
grid_affordability_mob <- grid_affordability_car |>
  rename(grid_id = id_grid_origin) |>
  select(grid_id, total_money) |>
  rename(affordability_car_total_money = total_money) |>
  left_join(grid_affordability_pt_pass |> rename(grid_id = id_grid_origin) |> select(grid_id, total_money) |> rename(affordability_transit_pass_total_money = total_money), by = "grid_id") |>
  left_join(grid_affordability_pt_single_fare |> rename(grid_id = id_grid_origin) |> select(grid_id, total_money) |> rename(affordability_transit_single_fare_total_money = total_money), by = "grid_id")

grid_income <- read_csv(IMPT_URL("/landuse/grid_income_housing_gini.csv")) |>
  select(-gini_coef) |>
  mutate(housing_costs_year = housing_costs * 12) # yearly costs

grid_affordability <- grid_affordability_mob |>
  left_join(grid_income, by = "grid_id") |>
  left_join(occ_rate_car_grid |> select(grid_id, taxa_ocup_auto), by = "grid_id") |>
  left_join(pop_movel_grid |> select(grid_id, pop_movel), by = "grid_id") |>
  left_join(households_size_grid |> select(grid_id, pp_hh_avg), by = "grid_id") |>
  filter(!is.na(taxa_ocup_auto)) |> # When no people, no affordability
  mutate(
    affordability_car_total_money = (affordability_car_total_money / taxa_ocup_auto) * pp_hh_avg * pop_movel, # adjust car costs by occupancy and household size and mobile pop
    affordability_transit_pass_total_money = affordability_transit_pass_total_money * pp_hh_avg * pop_movel, # adjust transit costs by household size and mobile pop
    affordability_transit_single_fare_total_money = affordability_transit_single_fare_total_money * pp_hh_avg * pop_movel # adjust transit costs by household size and mobile pop
  )

# Census modal share (used to compute modal-share-weighted affordability)
census_modal_share_grid <- read_csv(IMPT_URL("/census2021/census_modal_share_grid.csv")) |>
  select(grid_id = id, total, pt, private_vehicle, active, pt_share, private_vehicle_share, active_share)

# NAVEGANTE monthly pass scenario
Affordability_navegante_grid <- grid_affordability |>
  mutate(
    h_transp_inc_car = (housing_costs_year + affordability_car_total_money * trips_commuting_year) / income_hh,
    h_transp_inc_pt  = (housing_costs_year + affordability_transit_pass_total_money * trips_commuting_year) / income_hh,
    transp_inc_car   = (affordability_car_total_money * trips_commuting_year) / income_hh,
    transp_inc_pt    = (affordability_transit_pass_total_money * trips_commuting_year) / income_hh
  ) |>
  select(grid_id, h_transp_inc_car, h_transp_inc_pt, transp_inc_car, transp_inc_pt) |>
  left_join(census_modal_share_grid, by = "grid_id") |>
  mutate(
    h_transp_inc_comp = (h_transp_inc_car * private_vehicle_share + h_transp_inc_pt * pt_share) / (private_vehicle_share + pt_share + active_share),
    transp_inc_comp   = (transp_inc_car * private_vehicle_share + transp_inc_pt * pt_share) / (private_vehicle_share + pt_share + active_share)
  ) |>
  select(grid_id, h_transp_inc_car, h_transp_inc_pt, transp_inc_car, transp_inc_pt, h_transp_inc_comp, transp_inc_comp)

# SINGLE FARE scenario
Affordability_singlefare_grid <- grid_affordability |>
  mutate(
    h_transp_inc_car = (housing_costs_year + affordability_car_total_money * trips_commuting_year) / income_hh, # with housing costs
    h_transp_inc_pt  = (housing_costs_year + affordability_transit_single_fare_total_money * trips_commuting_year) / income_hh,
    transp_inc_car   = (affordability_car_total_money * trips_commuting_year) / income_hh, # without housing costs
    transp_inc_pt    = (affordability_transit_single_fare_total_money * trips_commuting_year) / income_hh
  ) |>
  select(grid_id, h_transp_inc_car, h_transp_inc_pt, transp_inc_car, transp_inc_pt) |>
  left_join(census_modal_share_grid, by = "grid_id") |>
  mutate( # accounting for modal share, with and without housing
    h_transp_inc_comp = (h_transp_inc_car * private_vehicle_share + h_transp_inc_pt * pt_share) / (private_vehicle_share + pt_share + active_share),
    transp_inc_comp   = (transp_inc_car * private_vehicle_share + transp_inc_pt * pt_share) / (private_vehicle_share + pt_share + active_share)
  ) |>
  select(grid_id, h_transp_inc_car, h_transp_inc_pt, transp_inc_car, transp_inc_pt, h_transp_inc_comp, transp_inc_comp)


# Export a comprehensive table of raw affordability variables before PCA, for grid
affordability_grid_composite <- grid_affordability |>
  select(
    grid_id, income_hh, housing_costs_year,
    affordability_car_total_money,
    affordability_transit_pass_total_money,
    affordability_transit_single_fare_total_money
  ) |>
  mutate(
    yearly_cost_car = affordability_car_total_money * trips_commuting_year,
    yearly_cost_pt_navegante = affordability_transit_pass_total_money * trips_commuting_year,
    yearly_cost_pt_singlefare = affordability_transit_single_fare_total_money * trips_commuting_year
  ) |>
  select(grid_id, income_hh, housing_costs_year, yearly_cost_car, yearly_cost_pt_navegante, yearly_cost_pt_singlefare) |>
  left_join(
    Affordability_navegante_grid |> select(
      grid_id,
      transp_inc_car,
      transp_inc_pt_nav = transp_inc_pt,
      transp_inc_comp_nav = transp_inc_comp,
      h_transp_inc_car,
      h_transp_inc_pt_nav = h_transp_inc_pt,
      h_transp_inc_comp_nav = h_transp_inc_comp
    ),
    by = "grid_id"
  ) |>
  left_join(
    Affordability_singlefare_grid |> select(
      grid_id,
      transp_inc_pt_sf = transp_inc_pt,
      transp_inc_comp_sf = transp_inc_comp,
      h_transp_inc_pt_sf = h_transp_inc_pt,
      h_transp_inc_comp_sf = h_transp_inc_comp
    ),
    by = "grid_id"
  ) |>
  left_join(census_modal_share_grid |> select(grid_id, private_vehicle_share, pt_share, active_share), by = "grid_id") |>
  mutate(
    yearly_cost_comp_nav = (yearly_cost_car * private_vehicle_share + yearly_cost_pt_navegante * pt_share) / (private_vehicle_share + pt_share + active_share),
    yearly_cost_comp_sf = (yearly_cost_car * private_vehicle_share + yearly_cost_pt_singlefare * pt_share) / (private_vehicle_share + pt_share + active_share)
  ) |>
  select(
    grid_id, income_hh, housing_costs_year,
    yearly_cost_car, yearly_cost_pt_navegante, yearly_cost_pt_singlefare, yearly_cost_comp_nav, yearly_cost_comp_sf,
    transp_inc_car, transp_inc_pt_nav, transp_inc_pt_sf, transp_inc_comp_nav, transp_inc_comp_sf,
    h_transp_inc_car, h_transp_inc_pt_nav, h_transp_inc_pt_sf, h_transp_inc_comp_nav, h_transp_inc_comp_sf
  )

write_csv(affordability_grid_composite, IMPT_URL("/affordability/affordability_grid_composite.csv"))
