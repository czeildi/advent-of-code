library(tidyverse)

input <- tibble(value = read_lines("solutions_2021/input22.txt")) %>% 
  extract(value, c("method", "x1", "x2", "y1", "y2", "z1", "z2"), regex = "([a-z]+) x=([0-9-]+)..([0-9-]+),y=([0-9-]+)..([0-9-]+),z=([0-9-]+)..([0-9-]+)", convert = TRUE) %>% 
  mutate(method = as.integer(method == "on"))

# part 1

input %>% 
  filter(x1 >= -50 & y1 >= -50 & z1 >= -50 & x2 <= 50 & y2 <= 50 & z2 <= 50) %>%
  rowwise() %>% 
  mutate(cube_points = list(crossing(x = x1:x2, y = y1:y2, z = z1:z2))) %>% 
  unnest(cube_points) %>% 
  select(x, y, z, method) %>% 
  group_by(x, y, z) %>% 
  summarize(
    final_value = tail(method, 1)
  ) %>% 
  ungroup() %>% 
  summarize(num_turned_on = sum(final_value))

# part 2

lights <- head(input, 1)