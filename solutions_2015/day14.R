library(tidyverse)
library(glue)

rd <-
"Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
Cupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds.
Prancer can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
Donner can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 11 km/s for 12 seconds, but then must rest for 125 seconds.
Comet can fly 21 km/s for 6 seconds, but then must rest for 121 seconds.
Blitzen can fly 18 km/s for 3 seconds, but then must rest for 50 seconds.
Vixen can fly 20 km/s for 4 seconds, but then must rest for 75 seconds.
Dancer can fly 7 km/s for 20 seconds, but then must rest for 119 seconds."

time <- 2503

tibble(x = str_split(rd, "\n")[[1]]) |>
  extract(x, c("velo", "travel", "rest"), regex = ".* (\\d+) .* (\\d+) .* (\\d+) .*", convert = TRUE) |>
  mutate(cycle_time = travel + rest) |>
  mutate(
    n_cycle = time %/% cycle_time,
    partial_cycle = time %% cycle_time
  ) |>
  mutate(partial_cycle_travel_time = pmin(partial_cycle, travel)) |>
  mutate(
    dist = n_cycle * travel * velo + partial_cycle_travel_time * velo
  ) |>
  pull(dist) |> max()
