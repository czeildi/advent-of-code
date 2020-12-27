library(tidyverse)

input <- read_lines("R8,U5,L5,D3\nU7,R6,D4,L4")
input <- read_lines(file = "solutions_2019/day03_input.txt")

step_params <- function(direction, distance) {
  list(
    distance = distance,
    x_by = if_else(direction %in% c("U", "D"), 0, if_else(direction == "R", 1, -1)),
    y_by = if_else(direction %in% c("R", "L"), 0, if_else(direction == "U", 1, -1))
  )
}

wires <- tibble(x = input) %>% 
  mutate(wire_id = 1:n()) %>% 
  separate_rows(x) %>% 
  extract(x, c("direction", "distance"), regex = "([RLUD])(\\d+)", convert = TRUE) %>% 
  rowwise() %>% 
  mutate(step = list(step_params(direction, distance))) %>% 
  ungroup() %>% 
  group_by(wire_id) %>% 
  mutate(step_id = 1:n()) %>% 
  group_split()
  
visited_coords <- map(wires, ~{
  pull(.x, step) %>% 
    reduce(.init = list(x = 0, y = 0, n_step = 0), function(visited, step) {
      new_x <- seq(tail(visited$x, 1) + step$x_by, length.out = step$distance, by = step$x_by)
      new_y <- seq(tail(visited$y, 1) + step$y_by, length.out = step$distance, by = step$y_by)
      list(
        x = c(visited$x, new_x),
        y = c(visited$y, new_y),
        n_step = c(visited$n_step, max(visited$n_step) + 1:step$distance)
      )
    }) %>% 
    {tibble(x = .$x, y = .$y, n_step = .$n_step)}
})

intersections <- inner_join(
  visited_coords[[1]],
  visited_coords[[2]],
  by = c("x", "y")
) %>% 
  filter(x != 0 | y != 0)


# part 1 ------------------------------------------------------------------

intersections %>% 
  mutate(manhattan_distance = abs(x) + abs(y)) %>% 
  pull(manhattan_distance) %>% 
  min()

# part 2 ------------------------------------------------------------------

intersections %>% 
  mutate(step_distance = n_step.x + n_step.y) %>% 
  pull(step_distance) %>% 
  min()
