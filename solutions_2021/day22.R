library(tidyverse)

input <- tibble(value = read_lines("solutions_2021/input22.txt")) %>% 
  extract(value, c("method", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax"), regex = "([a-z]+) x=([0-9-]+)..([0-9-]+),y=([0-9-]+)..([0-9-]+),z=([0-9-]+)..([0-9-]+)", convert = TRUE) %>% 
  mutate(method = as.integer(method == "on"))

# part 1

input %>% 
  filter(xmin >= -50 & ymin >= -50 & zmin >= -50 & xmax <= 50 & ymax <= 50 & zmax <= 50) %>%
  rowwise() %>% 
  mutate(cube_points = list(crossing(x = xmin:xmax, y = ymin:ymax, z = zmin:zmax))) %>% 
  unnest(cube_points) %>% 
  select(x, y, z, method) %>% 
  group_by(x, y, z) %>% 
  summarize(
    final_value = tail(method, 1),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  summarize(num_turned_on = sum(final_value))

# part 2

lights <- head(input, 0) %>% select(-method)
commands <- input

repeat {
  if(head(commands$method, 1) == 1) {
    lights <- rbind(lights, head(commands, 1) %>% select(-method))
    # transform into union of non-overlapping regions
  } else {
    # turn off intersection for each lighted region
  }
  commands <- tail(commands, -1)
  if (nrow(commands) == 0) break;
}

# add up region sizes


