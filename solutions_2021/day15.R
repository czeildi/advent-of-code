library(tidyverse)
library(glue)
library(igraph)

input <- read_lines("solutions_2021/input15.txt")

tile <- tibble(value = input) %>% 
  mutate(x = 1:n()) %>% 
  separate_rows(value, sep = "", convert = TRUE) %>% 
  filter(!is.na(value)) %>% 
  group_by(x) %>% 
  mutate(y = 1:n()) %>% 
  ungroup() %>% 
  mutate(node_id = 1:n())

tile_width = length(input)

tile_diffs <- tidyr::crossing(dx = 0:4, dy = 0:4)

cave_map <- map2(tile_diffs$dx, tile_diffs$dy, function(dx, dy) {
  tile %>% 
    mutate(
      x = x + dx * tile_width,
      y = y + dy * tile_width,
      value = if_else((value + dx + dy) %% 9 == 0, 9, (value + dx + dy) %% 9)
    )
}) %>% 
  bind_rows() %>% 
  mutate(node_id = 1:n())

directions <- tribble(
  ~direction, ~dx, ~dy,
  "right", 1, 0,
  "left", -1, 0,
  "bottom", 0, 1,
  "top", 0, -1
)

grid_w_neighbors <- cave_map %>% 
  inner_join(directions, by = character()) %>% 
  mutate(nb_x = x + dx, nb_y = y + dy) %>% 
  inner_join(cave_map, by = c("nb_x" = "x", "nb_y" = "y")) %>% 
  select(from = node_id.x, to = node_id.y, nb_value = value.y)

weighted_edges <- pmap(grid_w_neighbors, ~ c(..1, ..2, ..3)) %>% 
  unlist() %>% 
  matrix(ncol = 3, byrow = TRUE, .)

graph <- add_edges(
  make_empty_graph(nrow(cave_map)),
  t(weighted_edges[, 1:2]),
  weight=weighted_edges[, 3]
)

shortest_path <- as.integer(shortest_paths(graph, from = max(cave_map$node_id), to = 1, mode = "in")$vpath[[1]])

tibble(to = shortest_path, from = lead(shortest_path)) %>% 
  inner_join(grid_w_neighbors) %>% 
  pull(nb_value) %>% 
  sum()
