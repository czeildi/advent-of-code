library(tidyverse)

tibble(x = read_lines("solutions_2021/day02_input.txt")) |>
  separate(x, into = c("direction", "num"))


input <- tibble(x = read_lines("solutions_2021/day05_input.txt")) |>
  extract(x, c('ax', 'ay', 'bx', 'by'), regex = "(\\d+),(\\d+) -> (\\d+),(\\d+)", convert = TRUE)


directions <- tribble(
  ~direction, ~d_x, ~d_y,
  "right", 1, 0,
  "left", -1, 0,
  "bottom", 0, 1,
  "top", 0, -1
)

grid_w_neighbors <- input |>
  inner_join(directions, by = character()) |>
  mutate(nb_x = x + d_x, nb_y = y + d_y) |>
  left_join(input, by = c("nb_x" = "x", "nb_y" = "y")) |>
  rename(value = value.x, nb_value = value.y)


tile_diffs <- tidyr::crossing(dx = 0:4, dy = 0:4)


library(unglue)
library(zeallot)

input <- "target area: x=14..50, y=-267..-225"

c(x1, x2, y1, y2) %<-% unglue_data(input, "target area: x={x1}..{x2}, y={y1}..{y2}", convert = TRUE)
