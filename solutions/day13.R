library(tidyverse)

input <- read_lines('solutions/day13_input.txt')


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
  mutate(remainder_at_departure = (freq - target_diff) %% freq) %>%
  arrange(freq)

remainder_of_product <- function(base1, base2, r1, r2) {
  candidate <- r1
  while(candidate %% base2 != r2) {
    candidate <- candidate + base1
  }
  return(candidate)
}

base1 <- schedules$freq[1]
r1 <- schedules$remainder_at_departure[1]

for (row_index in 2:nrow(schedules)) {
  base2 <- schedules$freq[row_index]
  r2 <- schedules$remainder_at_departure[row_index]
  r1 <<- remainder_of_product(base1, base2, r1, r2)
  base1 <<- base1 * base2
}

sprintf("%.100f", r1)
