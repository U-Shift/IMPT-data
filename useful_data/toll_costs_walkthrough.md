# Toll Costs for Car OD Pairs — AML 2026

## Overview

This builds a `toll_costs_car_od.csv` with the toll cost (€, Classe 1, 2026) for each `from_id`/`to_id` pair in your r5r car itineraries. It works by matching the `osm_link_ids` column from `detailed_itineraries()` against a hand-curated lookup of OSM toll node IDs.

## Files Created

| File | Purpose |
|------|---------|
| [toll_costs_car.R](file:///media/rosa/Dados/GIS/IMPT-data/code/toll_costs_car.R) | Main R script — runs on your itinerary RDS files |
| [toll_plazas_aml_2026.csv](file:///media/rosa/Dados/GIS/IMPT-data/useful_data/toll_plazas_aml_2026.csv) | Reference table: plaza → OSM nodes → cost |

## Toll System: How It Works

The Portuguese motorway system in AML has two types:

| Type | How charged | Roads in AML |
|------|-------------|-------------|
| **Open** (pórtico) | Fixed fee each time you cross the gantry | Ponte 25 de Abril, Ponte Vasco da Gama, A8 (Loures-CREL), A16 |
| **Closed** (cabine/praça) | Fee based on **entry + exit** points | A1, A2, A5, A9 (CREL), A12 |

> [!IMPORTANT]
> Both bridges charge only **south → north** (Setúbal/Almada direction → Lisboa). If your r5r route crosses the bridge nodes going N→S, **no toll applies**. The script doesn't currently filter by direction — see the note below.

## 2026 Prices (Classe 1 = common car)

### Open tolls
| Plaza | Cost |
|-------|------|
| Ponte 25 de Abril (S→N) | **2.25 €** |
| Ponte Vasco da Gama (S→N) | **3.40 €** |
| A8 CREL–Lousa gantry | **0.75 €** |
| A16/IC30 gantry | **0.85 €** |

### Closed tolls (cumulative from entry point, Classe 1)

**A9 (CREL)** — from Estádio Nacional (A5/A9):
| Exit | Cost |
|------|------|
| → Queluz | 0.35 € |
| → A9/A16 | 0.65 € |
| → Radial Pontinha | 1.00 € |
| → A8/A9 | 1.40 € |
| → Bucelas | 1.75 € |
| → A9/A10 | 2.65 € |
| → Alverca | 3.00 € |

**A5** — from Cruz Quebrada (Lisbon end):
| Exit | Cost |
|------|------|
| → Oeiras | 0.40 € |
| → A16 junction | 0.70 € |
| → Estoril | 1.30 € |
| → Cascais | 1.60 € |

**A2** — from Fogueteiro (Almada):
| Exit | Cost |
|------|------|
| → Coina | 0.90 € |
| → Palmela | 1.75 € |

**A1** — from Alverca:
| Exit | Cost |
|------|------|
| → V.Franca de Xira II | 0.50 € |

**A12** — from Montijo:
| Exit | Cost |
|------|------|
| → Pinhal Novo | 1.10 € |
| → A2/A12 junction | 2.10 € |
| → Setúbal | 2.45 € |

## How to Run

1. **Adjust the input path** in [toll_costs_car.R](file:///media/rosa/Dados/GIS/IMPT-data/code/toll_costs_car.R):
   ```r
   itinerary_rds_path <- IMPT_URL("/mobility_fare_costs/itinerary_car_60min.rds")
   ```
   This should point to the RDS produced by your [mobility_costs_money.R](file:///media/rosa/Dados/GIS/IMPT-data/code/mobility_costs_money.R) script.

2. **Run the script** (after loading `r5r_network` and setting `IMPT_URL`):
   ```r
   source("code/toll_costs_car.R")
   ```

3. **Output**: `toll_costs_car_od.csv` saved to `IMPT_URL("/mobility_fare_costs/")`:
   ```
   from_id, to_id, toll_cost_eur
   ```

## Integration with mobility_costs_money.R

After computing tolls, merge into your itinerary and add the per-km cost:

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

## Known Limitations & Future Work

> [!NOTE]
> **Directional tolls**: Bridges are S→N only, but the script doesn't currently filter by travel direction. If needed, add a check on whether the route's origin is south of the bridge (lat < 38.70 for A2 Almada, lat < 38.67 for A12 Alcochete).

> [!NOTE]
> **OSM node completeness**: Not all toll booth nodes are perfectly tagged in OSM for AML. Some sections (especially A8 beyond Lousa, and A9 spur towards Arruda/Benavente) may have incomplete coverage. Check [toll_plazas_aml_2026.csv](file:///media/rosa/Dados/GIS/IMPT-data/useful_data/toll_plazas_aml_2026.csv) and add missing nodes as needed.

> [!TIP]
> **Verify with Via Verde simulator**: Cross-check computed costs against https://www.viaverde.pt/particulares/via-verde/onde-e-quanto-se-paga/simulador-de-portagens for specific origin–destination pairs.

> [!TIP]
> **r5r osm_link_ids format**: The column contains OSM **way** IDs (edges), not node IDs. If you need to match against **nodes**, use r5r's `street_network_to_sf()` to build a node-edge mapping, then convert.  
> Alternatively, query the OSM ways that contain each toll node (done above via Overpass) and use **way IDs** instead — see the bonus section below.

## Bonus: Matching by Way IDs Instead of Node IDs

If `osm_link_ids` returns way IDs (edges) rather than nodes, use this approach instead:

```r
# Query toll ways from OSM (the ways containing toll nodes)
toll_ways <- tribble(
  ~way_id,       ~plaza_id,
  "1042078066",  "pvascogama_sl",  # A12 with Vasco da Gama nodes
  "1042078306",  "a2_fogueteiro",  # A2 fogueteiro section
  # ... etc (see OSM query output for full list)
)

# Then in compute_toll_cost(), match osm_ids against toll_ways$way_id
```

The Overpass query results above already list which way IDs contain which toll nodes.
