library(tidyverse)
library(glue)
library(tidygraph)
options(scipen = 999)
source("utils.R")

year <- "2022"
day <- "12"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- read_lines(input_file) |> read_as_grid()
start_pos <- filter(input, value == "S") |>
  mutate(node_id = paste(x, y, sep = ";")) |> pull(node_id)
end_pos <- filter(input, value == "E") |>
  mutate(node_id = paste(x, y, sep = ";")) |> pull(node_id)

terrain <- input |>
  mutate(value = case_when(
    value == "S" ~ "a",
    value == "E" ~ "z",
    TRUE ~ value
  )) |>
  rowwise() |>
  mutate(value = utf8ToInt(value) - utf8ToInt("a"))


directions <- tribble(
  ~direction, ~d_x, ~d_y,
  "right", 1, 0,
  "left", -1, 0,
  "bottom", 0, 1,
  "top", 0, -1
)

grid_w_neighbors <- terrain %>%
  inner_join(directions, by = character()) %>%
  mutate(nb_x = x + d_x, nb_y = y + d_y) %>%
  left_join(terrain, by = c("nb_x" = "x", "nb_y" = "y")) %>%
  rename(value = value.x, nb_value = value.y)

edges <- grid_w_neighbors |>
  filter(nb_value <= value + 1) |>
  mutate(
    from = paste(x, y, sep = ";"),
    to = paste(nb_x, nb_y, sep = ";")
  ) |>
  select(from, to)

nodes <- unique(c(edges$from, edges$to))

graph <- tbl_graph(
  nodes = tibble(node_id = nodes),
  edges = edges
)

lowest_points <- terrain |>
  filter(value == 0) |>
  mutate(node_id = paste(x, y, sep = ";")) |>
  pull(node_id)

path_lengths <- sapply(seq_along(lowest_points), function(i) {
  message(i)
  start_node <- lowest_points[i]
  shortest_path <- graph %>%
    convert(to_shortest_path, node_id == start_node, node_id == end_pos)

  shortest_path |>
    activate(edges) |>
    as_tibble() |>
    nrow()
})

# returns 0 if no path
min(path_lengths[path_lengths > 0])
