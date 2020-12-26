library(tidyverse)

tibble(mass = as.numeric(read_lines("solutions_2019/day01_input.txt"))) %>%
  mutate(fuel = floor(mass / 3) - 2) %>% 
  pull(fuel) %>% 
  sum()
