library(tidyverse)
library(glue)
library(igraph)

input <- tibble(value = read_lines("solutions_2021/day09_input.txt")) %>% 
  mutate(x = 1:n()) %>% 
  separate_rows(value, sep = "", convert = TRUE) %>% 
  filter(!is.na(value)) %>% 
  group_by(x) %>% 
  mutate(y = 1:n()) %>% 
  ungroup()

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
  rename(value = value.x, nb_value = value.y)

# part 1

grid_w_neighbors %>% 
  mutate(is_smaller = is.na(nb_value) | value < nb_value) %>% 
  group_by(x, y, value) %>% 
  summarize(is_low_point = all(is_smaller)) %>% 
  ungroup() %>% 
  filter(is_low_point) %>% 
  summarize(total_risk = sum(value + 1))

# part 2

basin_edges <- grid_w_neighbors %>% 
  filter(value != 9 & nb_value != 9 & !is.na(nb_value)) %>% 
  mutate(from = glue("{x};{y}"), to = glue("{nb_x};{nb_y}"))

graph_edges_for_igraph <- map2(basin_edges$from, basin_edges$to, ~ c(.x, .y)) %>% unlist()
graph <- igraph::make_graph(graph_edges_for_igraph, directed = FALSE)

comps <- igraph::components(graph)
basin_sizes <- sort(comps$csize, decreasing = TRUE)

prod(head(basin_sizes, 3))