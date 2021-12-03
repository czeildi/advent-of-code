library(tidyverse)

x <- read_lines("solutions_2017/day01_input.txt") %>% 
  str_split("") %>% 
  .[[1]] %>% 
  as.numeric()

part1 <- c(x, head(x, 1))

sum(part1[part1 == lead(part1)], na.rm = TRUE)

part2 <- c(x, head(x, length(x) / 2))

sum(part2[part2 == lead(part2, length(x) / 2)], na.rm = TRUE)
