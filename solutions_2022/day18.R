library(tidyverse)
library(glue)
library(memoise)
options(scipen = 999)

year <- "2022"
day <- "18"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))
cubes <- input |>
  separate(x, c("x", "y", "z"), sep = ",", convert = TRUE)


directions <- tribble(
  ~direction, ~d_x, ~d_y, ~d_z,
  "xr", 1, 0, 0,
  "xl", -1, 0, 0,
  "yr", 0, 1, 0,
  "yl", 0, -1, 0,
  "zr", 0, 0, 1,
  "zl", 0, 0, -1,
)

grid_w_neighbors <- cubes |>
  inner_join(directions, by = character()) |>
  mutate(nb_x = x + d_x, nb_y = y + d_y, nb_z = z + d_z) |>
  left_join(mutate(cubes, dummy = 1), by = c("nb_x" = "x", "nb_y" = "y", "nb_z" = "z"))

grid_w_neighbors |>
  filter(is.na(dummy)) |>
  nrow()
