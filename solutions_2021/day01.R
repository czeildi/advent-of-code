library(tidyverse)

depths <- tibble(x = as.numeric(read_lines("solutions_2021/day01_input.txt")))

depths %>%
  mutate(increased = ((x - lag(x)) > 0)) %>%
  pull(increased) %>%
  sum(na.rm = TRUE)

# 1162

depths %>%
  mutate(window = x + lead(x) + lead(x, 2)) %>%
  mutate(increased = ((window - lag(window)) > 0)) %>%
  pull(increased) %>%
  sum(na.rm = TRUE)

# 1190
