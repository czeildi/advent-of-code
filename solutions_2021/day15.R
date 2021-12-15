library(tidyverse)
library(glue)

input <- tibble(value = read_lines("solutions_2021/input15_sample.txt")) %>% 
  mutate(x = 1:n()) %>% 
  separate_rows(value, sep = "", convert = TRUE) %>% 
  filter(!is.na(value)) %>% 
  group_by(x) %>% 
  mutate(y = 1:n()) %>% 
  ungroup() %>% 
  mutate(node_id = 1:n())

directions <- tribble(
  ~direction, ~d_x, ~d_y,
  "right", 1, 0,
  "left", -1, 0,
  "bottom", 0, 1,
  "top", 0, -1
)

grid_w_neighbors <- input %>% 
  inner_join(directions, by = character()) %>% 
  mutate(nb_x = x + d_x, nb_y = y + d_y) %>% 
  left_join(input, by = c("nb_x" = "x", "nb_y" = "y")) %>% 
  rename(value = value.x, nb_value = value.y) %>% 
  filter(!is.na(nb_value)) %>% 
  mutate(from = node_id.x, to = node_id.y) %>% 
  select(from, to, nb_value)

graph_edges_for_igraph <- pmap(grid_w_neighbors, ~ c(..1, ..2, ..3)) %>% unlist()

el <- matrix(ncol = 3, byrow = TRUE, graph_edges_for_igraph)

graph <- add_edges(
  make_empty_graph(nrow(input)),
  t(el[,1:2]),
  weight=el[,3]
)

dist <- distances(graph, to = 1, mode = "in")
tail(as.integer(dist), 1)
