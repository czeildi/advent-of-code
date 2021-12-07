library(tidyverse)

input <- scan("solutions_2021/day07_input.txt", sep = ",")

# part 1 submitted

sum(abs(input - median(input)))

# part 2 refactored

total_fuel_for_position <- function(position) {
  distances <- abs(input - position)
  sum(distances * (distances + 1) / 2)
}

map_dbl(1:max(input), ~total_fuel_for_position(.x)) %>% min()