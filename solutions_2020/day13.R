library(tidyverse)

input <- read_lines('solutions_2020/day13_input.txt')


# part 1 ------------------------------------------------------------------

arrival_time <- as.integer(input[1])

bus_schedules <- tibble(frequency = input[2]) %>%
  separate_rows(frequency) %>%
  filter(frequency != 'x') %>%
  mutate(frequency = as.integer(frequency)) %>%
  pull(frequency)

departure_time <- arrival_time

catched_buses <- function(departure_time) {
  bus_schedules[floor(departure_time/bus_schedules) == departure_time/bus_schedules]
}

while(length(catched_buses(departure_time)) == 0) {
  departure_time <<- departure_time + 1
}

catched_buses(departure_time) * (departure_time - arrival_time)


# part 2 ------------------------------------------------------------------

# sample solution: 1068781 

schedules <- tibble(freq = input[2]) %>%
  separate_rows(freq) %>%
  mutate(target_diff = 1:n() - 1) %>%
  filter(freq != 'x') %>%
  mutate(freq = as.numeric(freq)) %>%
  mutate(remainder_at_departure = (freq - target_diff) %% freq)

remainder_of_product <- function(x, y) {
  candidate <- x$r
  while(candidate %% y$base != y$r) {
    candidate <- candidate + x$base
  }
  return(candidate)
}

schedules %>%
  rowwise() %>%
  mutate(params = list(list('base' = freq, 'r' = remainder_at_departure))) %>%
  pull(params) %>%
  reduce(function(x, y){
    list('base' = x$base * y$base, 'r' = remainder_of_product(x, y))
  }) %>%
  `$`('r') %>%
  sprintf('%.0f', .)

