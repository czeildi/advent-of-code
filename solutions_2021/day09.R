library(tidyverse)
library(glue)

input <- tibble(x = read_lines("solutions_2021/day09_input.txt")) %>% 
  mutate(row_id = 1:n()) %>% 
  separate_rows(x, sep = "", convert = TRUE) %>% 
  filter(!is.na(x)) %>% 
  group_by(row_id) %>% 
  mutate(col_id = 1:n()) %>% 
  ungroup()

neighbors <- input %>% 
  group_by(row_id) %>% 
  mutate(
    left = lag(x),
    right = lead(x)
  ) %>% 
  ungroup() %>% 
  group_by(col_id) %>% 
  mutate(
    top = lag(x),
    bottom = lead(x)
  ) %>% 
  ungroup()

neighbors %>% 
  mutate(min_neighbors = pmin(left, right, bottom, top, na.rm = TRUE)) %>% 
  mutate(risk_level = if_else(min_neighbors > x, x + 1, 0)) %>% 
  summarize(result = sum(risk_level))

# part 2

basin_edges <- neighbors %>% 
  filter(x != 9) %>% 
  rowwise() %>% 
  mutate(edges = list(list(
    left_edge = list(fromx = row_id, from_y = col_id, to_x = row_id, to_y = col_id - 1, to_value = left),
    right_edge = list(fromx = row_id, from_y = col_id, to_x = row_id, to_y = col_id + 1, to_value = right),
    top_edge = list(fromx = row_id, from_y = col_id, to_x = row_id - 1, to_y = col_id, to_value = top),
    bottom_edge = list(fromx = row_id, from_y = col_id, to_x = row_id + 1, to_y = col_id, to_value = bottom)
  ))) %>% 
  select(edges) %>% 
  unnest(edges) %>% 
  unnest_auto(edges) %>% 
  filter(!is.na(to_value) & to_value != 9) %>% 
  mutate(from_point = glue("R{fromx}C{from_y}"), to_point = glue("R{to_x}C{to_y}"))

graph_edges_for_igraph <- map2(basin_edges$from_point, basin_edges$to_point, ~ c(.x, .y)) %>% unlist()

graph <- make_graph(graph_edges_for_igraph, directed = FALSE)

comps <- igraph::components(graph)

basin_sizes <- -sort(-comps$csize)

basin_sizes[1] * basin_sizes[2] * basin_sizes[3]