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