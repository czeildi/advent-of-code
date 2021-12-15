library(tidyverse)
library(glue)
library(igraph)

input <- tibble(value = read_lines("solutions_2021/input15.txt")) %>% 
  mutate(x = 1:n()) %>% 
  separate_rows(value, sep = "", convert = TRUE) %>% 
  filter(!is.na(value)) %>% 
  group_by(x) %>% 
  mutate(y = 1:n()) %>% 
  ungroup() %>% 
  mutate(node_id = 1:n())

tile_width = sqrt(nrow(input))

tile_diffs <- tidyr::crossing(dx = 0:4, dy = 0:4)

cave_map <- map2(tile_diffs$dx, tile_diffs$dy, function(dx, dy) {
  input %>% 
    rowwise() %>% 
    mutate(
      x = x + dx * tile_width,
      y = y + dy * tile_width,
      value = if_else((value + dx + dy) %% 9 == 0, 9, (value + dx + dy) %% 9)
    ) %>% 
    ungroup()
}) %>% 
  bind_rows() %>% 
  mutate(node_id = 1:n())

directions <- tribble(
  ~direction, ~d_x, ~d_y,
  "right", 1, 0,
  "left", -1, 0,
  "bottom", 0, 1,
  "top", 0, -1
)

grid_w_neighbors <- cave_map %>% 
  inner_join(directions, by = character()) %>% 
  mutate(nb_x = x + d_x, nb_y = y + d_y) %>% 
  left_join(cave_map, by = c("nb_x" = "x", "nb_y" = "y")) %>% 
  rename(value = value.x, nb_value = value.y) %>% 
  filter(!is.na(nb_value)) %>% 
  mutate(from = node_id.x, to = node_id.y) %>% 
  select(from, to, nb_value)

graph_edges_for_igraph <- pmap(grid_w_neighbors, ~ c(..1, ..2, ..3)) %>% unlist()

el <- matrix(ncol = 3, byrow = TRUE, graph_edges_for_igraph)

graph <- add_edges(
  make_empty_graph(nrow(cave_map)),
  t(el[,1:2]),
  weight=el[,3]
)

shortest_path <- as.integer(shortest_paths(graph, from = max(cave_map$node_id), to = 1, mode = "in")$vpath[[1]])

tibble(to = shortest_path, from = lead(shortest_path)) %>% 
  inner_join(grid_w_neighbors) %>% 
  pull(nb_value) %>% 
  sum()
