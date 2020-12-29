library(tidyverse)

orbits <- tibble(edge = read_lines("solutions_2019/day06_input.txt")) %>%
  extract(edge, into = c("center", "orbiter"), regex = "([A-Z0-9]+)\\)([A-Z0-9]+)")

centers_of_object_orbits <- deframe(select(orbits, orbiter, center))

# part 1 -----------------------------------------------------------------------

n_orbit_to_object <- function(object) {
  if (object == "COM") return(0)
  memoised_n_orbit_to_object(centers_of_object_orbits[[object]]) + 1
}

memoised_n_orbit_to_object <- memoise::memoise(n_orbit_to_object)

orbits %>%
  rowwise() %>%
  mutate(n_orbit = memoised_n_orbit_to_object(orbiter)) %>%
  pull(n_orbit) %>%
  sum()

# part 2 -----------------------------------------------------------------------

route_to_com <- function(object) {
  if (object == "COM") return(c("COM" = 0))
  current <- 0
  names(current) <- object
  c(current, route_to_com(centers_of_object_orbits[[object]]) + 1)
}

san_route <- route_to_com("SAN")
you_route <- route_to_com("YOU")

first_common_center <- intersect(names(san_route), names(you_route))[1]

san_route[first_common_center] - 1 + you_route[first_common_center] - 1
