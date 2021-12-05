library(tidyverse)

input <- tibble(direction = read_lines("solutions_2015/input03.txt")) %>% 
  separate_rows(direction, sep = "")

directions <- tribble(
  ~direction, ~x, ~y,
  ">", 1, 0,
  "<", -1, 0,
  "^", 0, 1,
  "v", 0, -1,
  "", 0, 0
)

input %>% 
  inner_join(directions) %>% 
  mutate(loc_x = cumsum(x), loc_y = cumsum(y)) %>% 
  mutate(house = glue::glue("X:{loc_x};Y:{loc_y}")) %>% 
  pull(house) %>% 
  n_distinct()

# part 2

input %>% 
  rbind(tibble(direction = ""), .) %>% 
  inner_join(directions) %>% 
  mutate(worker = rep(1:2, nrow(.)/2)) %>% 
  group_by(worker) %>% 
  mutate(loc_x = cumsum(x), loc_y = cumsum(y)) %>% 
  ungroup() %>% 
  mutate(house = glue::glue("X:{loc_x};Y:{loc_y}")) %>% 
  pull(house) %>% 
  n_distinct()