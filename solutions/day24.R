library(tidyverse)

input <- tibble(tile = read_lines("solutions/day24_input.txt"))

parse_line <- function(line) {
  if (line == '') return(character(0))
  first_direction <- str_extract(line, "(w|e|ne|nw|se|sw){1,1}")
  return(c(first_direction, parse_line(str_remove(line, first_direction))))
}

coord_diffs <- list(
  'e' = list(y_diff = 0, z_diff = 1),
  'w' = list(y_diff = 0, z_diff = -1),
  'ne' = list(y_diff = 1, z_diff = 0),
  'sw' = list(y_diff = -1, z_diff = 0),
  'nw' = list(y_diff = 1, z_diff = -1),
  'se' = list(y_diff = -1, z_diff = 1)
)

get_target_coords <- function(directions) {
  reduce(directions, .init = list(y = 0, z = 0), function(tile_coords, direction) {
    return(list(
      y = tile_coords$y + coord_diffs[[direction]]$y_diff,
      z = tile_coords$z + coord_diffs[[direction]]$z_diff
    ))
  })
}

tile_directions <- input %>% 
  rowwise() %>%
  mutate(directions = list(parse_line(tile))) %>%
  select(directions) %>%
  mutate(target_coords = list(get_target_coords(directions))) %>%
  ungroup()

tile_directions %>%
  count(target_coords) %>%
  mutate(is_flipped = n %% 2 == 1) %>%
  pull(is_flipped) %>%
  sum()
