library(tidyverse)

input <- tibble(value = read_lines("solutions_2021/input22.txt")) %>% 
  extract(value, c("method", "x1", "x2", "y1", "y2", "z1", "z2"), regex = "([a-z]+) x=([0-9-]+)..([0-9-]+),y=([0-9-]+)..([0-9-]+),z=([0-9-]+)..([0-9-]+)", convert = TRUE)

follow_lighting <- function(method_sequence) {
  reduce(method_sequence, function(prev, current) {
    if(current == "on") return(1);
    if(current == "off") return(0);
  }, .init = 0)
}

input %>% 
  filter(x1 >= -50 & y1 >= -50 & z1 >= -50 & x2 <= 50 & y2 <= 50 & z2 <= 50) %>%
  rowwise() %>% 
  mutate(cube_points = list(crossing(x = x1:x2, y = y1:y2, z = z1:z2))) %>% 
  unnest(cube_points) %>% 
  select(x, y, z, method) %>% 
  group_by(x, y, z) %>% 
  summarize(
    final_value = follow_lighting(method)
  ) %>% 
  ungroup() %>% 
  summarize(num_turned_on = sum(final_value))
