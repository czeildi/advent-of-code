library(tidyverse)
library(glue)
library(ggplot2)
options(scipen = 999)
theme_set(theme_bw())

year <- "2022"
day <- "14"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))

rock_line_points <- input |>
  mutate(rock_id = row_number()) |>
  separate_rows(x, sep = " -> ") |>
  group_by(rock_id) |>
  mutate(line_id = row_number()) |>
  separate(x, c("x", "y"), convert = TRUE)

floor_rock_id <- max(rock_line_points$rock_id) + 1
floor_y <- max(rock_line_points$y) + 2
floor_left_x <- 500 - floor_y - 1
floor_right_x <- 500 + floor_y + 1
floor_rock <- tibble::tribble(
  ~x, ~y, ~rock_id, ~line_id,
  floor_left_x, floor_y, floor_rock_id, 1,
  floor_right_x, floor_y, floor_rock_id, 2,
)

cave <- rock_line_points |>
  rbind(floor_rock) |>
  mutate(xp = lag(x), yp = lag(y)) |>
  rowwise() |>
  mutate(xp = replace_na(xp, x), yp = replace_na(yp, y)) |>
  summarise(x = list(x:xp), y = list(y:yp), .groups = "drop") |>
  unnest_longer(col = c(x, y)) |>
  distinct(x, y) |>
  ungroup()

ggplot(cave, aes(x, -y)) +
  geom_tile() +
  coord_fixed() +
  expand_limits(y = 0) +
  geom_vline(xintercept = 500, linetype = "dashed")

grid <- crossing(x = min(cave$x):max(cave$x), y = 0:max(cave$y))

area <- cave |>
  mutate(material = "R") |>
  right_join(grid, by = c("x", "y")) |>
  mutate(material = replace_na(material, "-"), coord = paste(x, y, sep = ";")) |>
  select(coord, material) |>
  deframe()

area <- lapply(area, \(cc) list(v = cc, prev = NA_character_))

coord <- function(coords) paste(coords, collapse = ";")
sand_position <- c(500, 0)
sand_counter <- 0
area[[coord(sand_position)]] <- c(500, -1)

repeat {
  if (area[[coord(sand_position + c(0, 1))]]$v == "-") {
    area[[coord(sand_position + c(0, 1))]]$prev <- sand_position
    sand_position <- sand_position + c(0, 1)
  } else if (area[[coord(sand_position + c(-1, 1))]]$v == "-") {
    area[[coord(sand_position + c(-1, 1))]]$prev <- sand_position
    sand_position <- sand_position + c(-1, 1)
  } else if (area[[coord(sand_position + c(1, 1))]]$v == "-") {
    area[[coord(sand_position + c(1, 1))]]$prev <- sand_position
    sand_position <- sand_position + c(1, 1)
  } else {
    area[[coord(sand_position)]]$v <- "S"
    sand_counter <- sand_counter + 1
    if (coord(sand_position) == "500;0") break
    sand_position <- area[[coord(sand_position)]]$prev
  }
}

Filter(\(x) x == "S", area) |> length()
