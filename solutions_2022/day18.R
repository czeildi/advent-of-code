library(tidyverse)
library(glue)
library(memoise)
library(tidygraph)
options(scipen = 999)

year <- "2022"
day <- "18"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))
cubes <- input |>
  separate(x, c("x", "y", "z"), sep = ",", convert = TRUE) |>
  mutate(id = row_number())


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
  left_join(cubes, by = c("nb_x" = "x", "nb_y" = "y", "nb_z" = "z")) |>
  rename(id = id.x, nb_id = id.y)

# part 1
grid_w_neighbors |>
  filter(is.na(nb_id)) |>
  nrow()

# part 2

expanded <- cubes |>
  right_join(
    crossing(
      x = (min(cubes$x) - 1):(max(cubes$x) + 1),
      y = (min(cubes$y) - 1):(max(cubes$y) + 1),
      z = (min(cubes$z) - 1):(max(cubes$z) + 1)
    ),
    by = c("x", "y", "z")
  ) |>
  mutate(is_air = is.na(id), id = row_number())

w_neighbors <- expanded |>
  inner_join(directions, by = character()) |>
  mutate(nb_x = x + d_x, nb_y = y + d_y, nb_z = z + d_z) |>
  left_join(expanded, by = c("nb_x" = "x", "nb_y" = "y", "nb_z" = "z")) |>
  rename(id = id.x, nb_id = id.y, is_air = is_air.x, nb_is_air = is_air.y) |>
  select(-d_x, -d_y, -d_z, -direction)

nodes <- w_neighbors |>
  mutate(name = paste(x, y, z, sep = ";")) |>
  select(node_id = id, name) |>
  unique()

edges <- w_neighbors |>
  filter(is_air == nb_is_air) |>
  select(from = id, to = nb_id)

g <- tbl_graph(nodes = nodes, edges = edges)

connected_components <- g |>
  activate(nodes) |>
  mutate(group = group_components()) |>
  activate(nodes) |>
  as_tibble()

outer_component_id <- connected_components |>
  filter(name == paste((min(cubes$x) - 1), (min(cubes$y) - 1), (min(cubes$z) - 1), sep = ";")) |>
  pull(group)

trapped_air <- expanded |>
  inner_join(connected_components, by = c("id" = "node_id")) |>
  filter(is_air & group != outer_component_id) |>
  select(id, name, group)

w_neighbors |>
  filter(!is_air & nb_is_air) |>
  anti_join(trapped_air, by = c("nb_id" = "id")) |>
  nrow()
