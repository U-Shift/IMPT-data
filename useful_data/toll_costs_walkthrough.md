# Toll Costs for Car OD Pairs — AML 2026

## Overview

This builds a `toll_costs_car_od.csv` with the toll cost (€, Classe 1, 2026) for each `from_id`/`to_id` pair in your r5r car itineraries. It works by matching the `osm_link_ids` column from `detailed_itineraries()` against a curated lookup of OSM **way** IDs for toll infrastructure.

## Files

| File | Purpose |
|------|---------|
| `code/toll_costs_car.R` | Main R script — reads your r5r RDS, outputs `toll_costs_car_od.csv` |
| `useful_data/toll_plazas_aml_2026.csv` | **Source of truth** — plaza → OSM way IDs → 2026 cost. Edit here to add/fix ways. |

## Why OSM Way IDs (not node IDs)?

r5r's `detailed_itineraries(osm_link_ids = TRUE)` returns OSM **way** IDs (road edges), not node IDs. The correct identifiers to match against are therefore the ways that represent the toll infrastructure:

| Way ID | Name | Tags |
|--------|------|------|
| `22286596` | Ponte 25 de Abril | `ref=A 2`, `toll=yes`, `oneway=yes` |
| `405372511` | Ponte Vasco da Gama | `ref=PVG;IP 1`, `toll=yes`, `oneway=yes` |

Because both bridge ways are `oneway=yes` (S→N), r5r will only route through them in the payable direction — **no extra directional logic is needed**.

## Toll System: How It Works

The Portuguese motorway system in AML has two types:

| Type | How charged | Roads in AML |
|------|-------------|-------------|
| **Open** (pórtico/free-flow) | Fixed fee each time a tagged bridge/gantry way is traversed | Ponte 25 de Abril, Ponte Vasco da Gama, A8 (Loures–CREL), A16 |
| **Closed** (praça/cabine) | Fee based on **entry + exit** plaza pair (cumulative per section) | A1, A2, A5, A9 (CREL), A12 |

## 2026 Prices (Classe 1 = common car)

Source: [ACP Preços Portagens 2026](https://www.acp.pt/ResourcesUser/ACP/docs/Viagens_e_Lazer/Estrada_fora/Precos-Portagens-2026.pdf)

### Open tolls

| Plaza ID | Way ID(s) | Cost |
|----------|-----------|------|
| `p25abril_sl` | `22286596` | **2.25 €** |
| `pvascogama_sl` | `405372511` | **3.40 €** |
| `a8_crel_lousa` | `1433748419` … | **0.75 €** |
| `a16_gantry` | `1422902996` … | **0.85 €** |

### Closed tolls — cumulative costs from entry point (Classe 1)

**A9 (CREL)** — from `a9_estadio` (Estádio Nacional / A5 junction):

| Exit plaza | Cost |
|------------|------|
| `a9_queluz` | 0.35 € |
| `a9_a16_pontinha` | 0.65 € |
| `a9_radial_pontinha` | 1.00 € |
| `a9_a8_junction` | 1.40 € |
| `a9_bucelas` | 1.75 € |
| `a9_a10_junction` | 2.65 € |
| `a9_alverca` | 3.00 € |

**A5** — from `a5_cruz_quebrada` (Cruz Quebrada / Lisbon end):

| Exit plaza | Cost |
|------------|------|
| `a5_oeiras` | 0.40 € |
| `a5_a16_junction` | 0.70 € |
| `a5_estoril` | 1.10 € |
| `a5_cascais` | 1.60 € |

**A2** — from `a2_fogueteiro` (Fogueteiro / Almada):

| Exit plaza | Cost |
|------------|------|
| `a2_coina` | 0.90 € |
| `a2_palmela` | 1.75 € |

**A1** — from `a1_alverca`:

| Exit plaza | Cost |
|------------|------|
| `a1_vfxira_ii` | 0.50 € |

**A12** — from `a12_montijo`:

| Exit plaza | Cost |
|------------|------|
| `a12_pinhal_novo` | 1.10 € |
| `a12_a2jct` | 2.10 € |
| `a12_setubal` | 2.45 € |

## How to Run

1. **Adjust the input path** in `code/toll_costs_car.R`:
   ```r
   itinerary_rds_path <- IMPT_URL("/mobility_fare_costs/itinerary_car_60min.rds")
   ```
   This should point to the RDS produced by `code/mobility_costs_money.R`.

2. The script reads `toll_plazas_aml_2026.csv` automatically via `IMPT_URL("/useful_data/toll_plazas_aml_2026.csv")`.

3. **Run** (after `r5r_network` and `IMPT_URL` are set):
   ```r
   source("code/toll_costs_car.R")
   ```

4. **Output** saved to `IMPT_URL("/mobility_fare_costs/toll_costs_car_od.csv")`:
   ```
   from_id, to_id, toll_cost_eur
   ```

## Integration with `mobility_costs_money.R`

After computing tolls, merge into your itinerary:

```r
cost_per_km <- 0.4  # €/km (Portaria 1553-D/2008)

itineraries_with_cost <- itineraries %>%
  left_join(od_toll_costs, by = c("from_id", "to_id")) %>%
  mutate(
    toll_cost_eur = replace_na(toll_cost_eur, 0),
    distance_km   = distance / 1000,
    car_cost_eur  = distance_km * cost_per_km + toll_cost_eur
  )
```

## Adding or Correcting Toll Ways

To add a missing toll way, edit `useful_data/toll_plazas_aml_2026.csv`:

1. Find the way ID on [openstreetmap.org](https://www.openstreetmap.org) — look for `toll=yes` ways on the relevant motorway
2. Add the way ID to the `osm_way_ids` column (semicolon-separated) of the correct plaza row
3. Re-run `toll_costs_car.R` — the script reads the CSV fresh each time, no code changes needed

> [!TIP]
> To find way IDs for a given plaza, use [Overpass Turbo](https://overpass-turbo.eu):
> ```
> way["toll"="yes"]["ref"="A 9"](38.3,-9.5,38.9,-8.7);
> out body;
> ```
> Replace `"A 9"` with your target road ref, then cross-reference with the map.

## Known Limitations

> [!NOTE]
> **Closed system way coverage**: Way IDs listed per plaza correspond to segments at or near the toll collection point. Since r5r may route via adjacent or parallel ways, several nearby way IDs are listed per plaza (see the CSV) to improve robustness.

> [!NOTE]
> **A8 (Loures–CREL is free)**: The Loures–CREL section costs 0.00 €. Only the CREL–Lousa section (0.75 €) is charged. This is correctly reflected in the CSV.

> [!TIP]
> **Verify with Via Verde**: Cross-check computed costs against the [Via Verde toll simulator](https://www.viaverde.pt/particulares/via-verde/onde-e-quanto-se-paga/simulador-de-portagens) for specific OD pairs.
