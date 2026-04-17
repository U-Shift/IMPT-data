# grid test

# aim: get the statistical distribution of the number of cell grid at each freguesia
library(sf)
library(tidyverse)


## With 8
grid_8_centroids = points_h3
nrow(grid_8_centroids) # 3686

freg_point_count_8 = freguesias |> st_join(grid_8_centroids, join = st_contains) |>
  st_drop_geometry() |>
  count(dtmnfr, name = "n_points")

freguesias_stats_8 <- freguesias |>
  left_join(freg_point_count_8, by = "dtmnfr") |>
  mutate(n_points = replace_na(n_points, 0))

summary(freguesias_stats_8$n_points)
freguesias_stats_8 |>
  st_drop_geometry() |>
  summarise(
    total_points = sum(n_points),
    mean_points  = mean(n_points),
    median_points = median(n_points),
    min_points = min(n_points),
    max_points = max(n_points),
    sd_points = sd(n_points)
  )

ggplot(freguesias_stats_8, aes(n_points)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(title = "Distribution of points per freguesia",
       x = "Number of points",
       y = "Number of freguesias") +
  theme_minimal()
ggplot(freguesias_stats_8) +
  geom_sf(aes(fill = n_points)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Points by Freguesia - res 8", fill = "Points") +
  theme_minimal()

## With 9
grid_9_centroids = points_h3_9
nrow(grid_9_centroids) # 25890

freg_point_count_9 = freguesias |> st_join(grid_9_centroids, join = st_contains) |>
  st_drop_geometry() |>
  count(dtmnfr, name = "n_points")

freguesias_stats_9 <- freguesias |>
  left_join(freg_point_count_9, by = "dtmnfr") |>
  mutate(n_points = replace_na(n_points, 0))

summary(freguesias_stats_9$n_points)
freguesias_stats_9 |>
  st_drop_geometry() |>
  summarise(
    total_points = sum(n_points),
    mean_points  = mean(n_points),
    median_points = median(n_points),
    min_points = min(n_points),
    max_points = max(n_points),
    sd_points = sd(n_points)
  )

ggplot(freguesias_stats_9, aes(n_points)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(title = "Distribution of points per freguesia",
       x = "Number of points",
       y = "Number of freguesias") +
  theme_minimal()
ggplot(freguesias_stats_9) +
  geom_sf(aes(fill = n_points)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Points by Freguesia - res 9", fill = "Points") +
  theme_minimal()
