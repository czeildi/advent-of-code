library(tidyverse)
library(numbers)

asteroids <- tibble(location = read_lines("./solutions_2019/day10_input.txt")) %>%
  mutate(y = 0:(n() - 1)) %>%
  separate_rows(location, sep = "") %>%
  filter(location != "") %>%
  group_by(y) %>%
  mutate(x = 0:(n() - 1)) %>%
  ungroup() %>%
  filter(location == "#") %>%
  select(x, y)

asteroids_from_centers <- crossing(station = asteroids, asteroid = asteroids) %>%
  filter(station$x != asteroid$x | station$y != asteroid$y) %>%
  mutate(station_x = station$x, station_y = station$y, asteroid_x = asteroid$x, asteroid_y = asteroid$y) %>%
  select(station_x, station_y, asteroid_x, asteroid_y) %>%
  mutate(distance_x = asteroid_x - station_x, distance_y = asteroid_y - station_y) %>%
  rowwise() %>%
  mutate(n_step = GCD(distance_x, distance_y)) %>%
  mutate(
    direction_x = distance_x / n_step,
    direction_y = distance_y / n_step
  ) %>%
  ungroup()

# part 1 --------------------------------------------------------------------
monitoring_station_coords <- asteroids_from_centers %>%
  group_by(station_x, station_y) %>%
  summarise(n_visible_asteroid = n_distinct(glue::glue("{direction_x};{direction_y}"))) %>%
  arrange(desc(n_visible_asteroid)) %>%
  head(1) %>%
  select(x = station_x, y = station_y, n_visible_asteroid)

monitoring_station_coords$n_visible_asteroid

# part 2 ---------------------------------------------------------------------

asteroids_from_centers %>%
  filter(station_x == monitoring_station_coords$x & station_y == monitoring_station_coords$y) %>%
  select(direction_x, direction_y, n_step, asteroid_x, asteroid_y) %>%
  group_by(direction_x, direction_y) %>%
  arrange(n_step) %>%
  mutate(tally_in_direction = 1:n()) %>%
  rowwise() %>%
  mutate(
    rotation = case_when(
      direction_x >= 0 ~ glue::glue("A:{direction_y / direction_x}"),
      direction_x < 0 ~ glue::glue("B:{direction_y / direction_x}")
    )
  ) %>%
  ungroup() %>%
  arrange(tally_in_direction, rotation) %>%
  mutate(laser_idx = 1:n()) %>%
  filter(laser_idx == 200)
