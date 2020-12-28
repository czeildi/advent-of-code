library(tidyverse)

module_masses <- tibble(mass = as.numeric(read_lines("solutions_2019/day01_input.txt")))

# part 1 ------------------------------------------------------------------

module_masses %>%
  mutate(fuel = floor(mass / 3) - 2) %>%
  pull(fuel) %>%
  sum()

# part 2 ------------------------------------------------------------------

fuel_for_mass <- function(mass) {
  accumulate(c(mass, rep(-1, 100)), function(fuel_mass, dummy) {
    needed_fuel <- floor(fuel_mass / 3) - 2
    if (needed_fuel <= 0) return(done(0))
    needed_fuel
  }) %>%
    tail(-1) %>%
    sum()

}

module_masses %>%
  rowwise() %>%
  mutate(fuel = fuel_for_mass(mass)) %>%
  pull(fuel) %>%
  sum()

