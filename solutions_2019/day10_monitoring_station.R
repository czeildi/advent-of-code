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

crossing(station = asteroids, asteroid = asteroids) %>%
  filter(station$x != asteroid$x | station$y != asteroid$y) %>%
  mutate(station_x = station$x, station_y = station$y, asteroid_x = asteroid$x, asteroid_y = asteroid$y) %>%
  select(station_x, station_y, asteroid_x, asteroid_y) %>%
  mutate(distance_x = asteroid_x - station_x, distance_y = asteroid_y - station_y) %>%
  rowwise() %>%
  mutate(direction = str_c(c(distance_x, distance_y) / GCD(distance_x, distance_y), collapse = ";")) %>%
  ungroup() %>%
  group_by(station_x, station_y) %>%
  summarise(n_visible_asteroid = n_distinct(direction)) %>%
  arrange(desc(n_visible_asteroid))
