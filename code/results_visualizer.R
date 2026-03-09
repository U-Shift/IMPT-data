library(mapview)
freguesias # Read from data_load.R

# IMPT Equal Weights
impt_ew = read.csv(IMPT_URL("results_aggregated/IMPT_Equal_Weights.csv"))

# IMPT PCA and Entropy scores
impt_pca = read.csv(IMPT_URL("results_aggregated/IMPT_PCA_and_Entropy_Scores.csv"))
impt_pca_sf = freguesias |>
  select(-nuts2, -area_ha) |>
  mutate(dtmnfr = as.integer(dtmnfr)) |>
  left_join(
    impt_pca |>
      select(-IMPT_score_pca) |> # Duplicated column
      rename(
        Accessibility_PCA = Accessibility_Index,
        Affordability_PCA = Affordability_Index,
        Mobility_PCA = Mobility_Index,
        Safety_PCA = Safety_Index
      )
  ) |> 
  left_join(impt_ew |> 
      select(Accessibility, Affordability, Mobility, Safety, dtmnfr, IMPT_Equal_Weights_avg) |>
      rename(Accessibility_EA = Accessibility, Affordability_EA = Affordability, Mobility_EA = Mobility, Safety_EA = Safety)
    ) |>
  # Round PCA scores to 2 decimals
  mutate(
    across(starts_with("Accessibility"), ~ round(., 2)),
    across(starts_with("Affordability"), ~ round(., 2)),
    across(starts_with("Mobility"), ~ round(., 2)),
    across(starts_with("Safety"), ~ round(., 2)),
    across(starts_with("IMPT"), ~ round(., 2))
  ) |>
  # Columns order
  select(
    dtmnfr, freguesia, municipio, 
    starts_with("Accessibility"), starts_with("Affordability"), starts_with("Mobility"), starts_with("Safety"),
    starts_with("IMPT")
  )
# View(impt_pca_sf |> st_drop_geometry())

mapview(impt_pca_sf, zcol="Accessibility_PCA", layer.name="PCA Accessibility", hide=TRUE) +
  mapview(impt_pca_sf, zcol="Affordability_PCA", layer.name="PCA Affordability", hide=TRUE) +
  mapview(impt_pca_sf, zcol="Mobility_PCA", layer.name="PCA Mobility", hide=TRUE) +
  mapview(impt_pca_sf, zcol="Safety_PCA", layer.name="PCA Safety", hide=TRUE) +
  
  mapview(impt_pca_sf, zcol="Accessibility_EA", layer.name="EA Accessibility", hide=TRUE) +
  mapview(impt_pca_sf, zcol="Affordability_EA", layer.name="EA Affordability", hide=TRUE) +
  mapview(impt_pca_sf, zcol="Mobility_EA", layer.name="EA Mobility", hide=TRUE) +
  mapview(impt_pca_sf, zcol="Safety_EA", layer.name="EA Safety", hide=TRUE) +

  mapview(impt_pca_sf, zcol="IMPT_score_pca_geom", layer.name="IMPT PCA Geom", hide=TRUE) + 
  mapview(impt_pca_sf, zcol="IMPT_score_pca_avg", layer.name="IMPT PCA Avg", hide=TRUE) + 
  mapview(impt_pca_sf, zcol="IMPT_entropy_pca", layer.name="IMPT PCA Entropy", hide=TRUE) +
  mapview(impt_pca_sf, zcol="IMPT_Equal_Weights", layer.name="IMPT EA + Geom", hide=TRUE) +
  mapview(impt_pca_sf, zcol="IMPT_Equal_Weights_avg", layer.name="IMPT EA + Avg", hide=TRUE) 
