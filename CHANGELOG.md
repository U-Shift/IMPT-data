# Changelog

## 04/03/2026

- `/mobility_transit/<grid/freguesias/municipios>_headways.gpkg`
  - Added new columns with measures for night and weekend services: `weighted_headway_<night/weekend>`, `weighted_waiting_time_<night/weekend>`, `weighted_frequency_<night/weekend>`, `total_frequency_<night/weekend>`
  - Created new columns with night and weekend service ratios: `frequency_reduction_<night/weekend>` and `weighted_frequency_reduction_<night/weekend>`
  
- `/mobility_commuting/<grid/freguesias/municipios>_transfers.gpkg`
  - Recalculated `weighted_mean_transfers` and `total_transfers` to consider transit trips with no transfers (before we were not considering direct trips, without transfers, and this was overestimating the average number of transfers for some areas)
  
- `/mobility_commuting/<grid/freguesias/municipios>_commuting.gpkg`
   - Travel time extended with more transit travel time matrices (more walking time, cut off and transfers variations) (the ones calculated previously remain unchanged)