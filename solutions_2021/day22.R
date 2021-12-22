library(tidyverse)
library(data.table)

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

x_breakpoints <- sort(unique(c(input$xmin, input$xmax)))

x_intervals <- map2(x_breakpoints, lead(x_breakpoints), ~ {
  if(is.na(.y)) return(NULL)
  else return(list(xmin = .x + 1, xmax = .y - 1))
}) %>% 
  compact() %>% 
  c(., map(x_breakpoints, ~list(xmin = .x, xmax = .x))) %>% 
  enframe() %>% 
  select(-name) %>% 
  unnest_wider(value) %>% 
  arrange(xmin) %>% 
  as.data.table() %>% 
  .[, dummy := 1]

y_breakpoints <- sort(unique(c(input$ymin, input$ymax)))

y_intervals <- map2(y_breakpoints, lead(y_breakpoints), ~ {
  if(is.na(.y)) return(NULL)
  else return(list(ymin = .x + 1, ymax = .y - 1))
}) %>% 
  compact() %>% 
  c(., map(y_breakpoints, ~list(ymin = .x, ymax = .x))) %>% 
  enframe() %>% 
  select(-name) %>% 
  unnest_wider(value) %>% 
  arrange(ymin) %>% 
  as.data.table() %>% 
  .[, dummy := 1]

z_breakpoints <- sort(unique(c(input$zmin, input$zmax)))

z_intervals <- map2(z_breakpoints, lead(z_breakpoints), ~ {
  if(is.na(.y)) return(NULL)
  else return(list(zmin = .x + 1, zmax = .y - 1))
}) %>% 
  compact() %>% 
  c(., map(z_breakpoints, ~list(zmin = .x, zmax = .x))) %>% 
  enframe() %>% 
  select(-name) %>% 
  unnest_wider(value) %>% 
  arrange(zmin) %>% 
  as.data.table() %>% 
  .[, dummy := 1]

volume <- 0

for (i in 1:nrow(x_intervals)) {
  print(paste(Sys.time(), "checking:", i))
  volume_in_x_crosssection <- input %>% 
    rename(x1 = xmin, x2 = xmax, y1 = ymin, y2 = ymax, z1 = zmin, z2 = zmax) %>% 
    mutate(dummy = 1) %>% 
    as.data.table() %>% 
    merge(x_intervals[i, ], by = "dummy", allow.cartesian = TRUE) %>% 
    .[xmin >= x1 & xmax <= x2, ] %>% 
    merge(y_intervals, by = "dummy", allow.cartesian = TRUE) %>% 
    .[ymin >= y1 & ymax <= y2] %>% 
    merge(z_intervals, by = "dummy", allow.cartesian = TRUE) %>% 
    .[zmin >= z1 & zmax <= z2] %>% 
    .[, .(last_light = last(method)), by = .(xmin, xmax, ymin, ymax, zmin, zmax)] %>% 
    .[last_light == 1,] %>% 
    .[, volume := (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1)] %>% 
    .[, sum(volume)]
  volume <- volume + volume_in_x_crosssection
}


volume
# sample expected: 2758514936282235