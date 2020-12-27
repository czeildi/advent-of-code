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
  
visited_coords <- wires %>% 
  map(~{
    .x %>% 
      pull(step) %>% 
      reduce(.init = list(x = 0, y = 0), function(visited, step) {
        end_x <- tail(visited$x, 1)
        end_y <- tail(visited$y, 1)
        list(
          x = c(visited$x, seq(end_x + step$x_by, length.out = step$distance, by = step$x_by)),
          y = c(visited$y, seq(end_y + step$y_by, length.out = step$distance, by = step$y_by))
        )
      }) %>% 
      {tibble(x = .$x, y = .$y)}
  })

inner_join(
  visited_coords[[1]],
  visited_coords[[2]],
  by = c("x", "y")
) %>% 
  filter(x != 0 | y != 0) %>% 
  mutate(manhattan_distance = abs(x) + abs(y)) %>% 
  pull(manhattan_distance) %>% 
  min()
