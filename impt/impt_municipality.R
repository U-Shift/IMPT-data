# Using variables from dashboard_export.R
impt_pca_freguesia = impt_pca |>
  left_join(impt_pca_bike, by="dtmnfr") |>
  left_join(impt_pca_walk, by="dtmnfr") |>
  left_join(impt_pca_pt, by="dtmnfr") |>
  left_join(impt_pca_car, by="dtmnfr") 

census_pop = census |> # From data_load.R
  select(dicofre24, N_INDIVIDUOS) |>
  filter(!is.na(dicofre24)) |>
  rename(pop = N_INDIVIDUOS) |>
  st_drop_geometry() |>
  rename(dtmnfr = dicofre24) |>
  mutate(dtmnfr = as.integer(dtmnfr))

impt_pca_municipality = impt_pca_freguesia |>
  left_join(census_pop, by="dtmnfr") |>
  left_join(freg_nuts |> select(id, group_id) |> rename(dtmnfr=id, mun_id=group_id), by="dtmnfr") |>
  group_by(mun_id) |>
  summarise(
    across(
      contains("_Index"),
      ~ weighted.mean(.x, w=pop, na.rm=TRUE)
    ),
    across(
      contains("IMPT_"),
      ~ weighted.mean(.x, w=pop, na.rm=TRUE)
    )
  )

write.csv(impt_pca_municipality, IMPT_URL("results_aggregated/20260330/impt_municipality.csv"), row.names=FALSE)
