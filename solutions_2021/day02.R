library(tidyverse)

tibble(x = read_lines("solutions_2021/day02_input.txt")) %>% 
  separate(x, into = c("direction", "num")) %>% 
  mutate(num = as.numeric(num)) %>% 
  group_by(direction) %>% 
  summarise(y = sum(num))

# part 2

tibble(x = read_lines("solutions_2021/day02_input.txt")) %>% 
  separate(x, into = c("direction", "num")) %>% 
  mutate(num = as.numeric(num)) %>% 
  mutate(aim_increase = case_when(
    direction == 'down' ~ num,
    direction == 'up' ~ -num,
    TRUE ~ 0
  )) %>% 
  mutate(aim = cumsum(aim_increase)) %>% 
  mutate(depth_increase = case_when(
    direction == 'forward' ~ num,
    TRUE ~ 0
  )) %>% 
  mutate(forward_increase = case_when(
    direction == 'forward' ~ aim * num,
    TRUE ~ 0
  )) %>% 
  mutate(depth = cumsum(depth_increase), forward = cumsum(forward_increase)) %>% 
  select(depth, forward) %>% 
  tail(1)
