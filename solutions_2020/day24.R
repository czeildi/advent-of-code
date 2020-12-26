library(tidyverse)

input <- tibble(tile = read_lines("solutions_2020/day24_input.txt"))

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
  filter(n %% 2 == 1) %>%
  nrow()

# part 2 ------------------------------------------------------------------

black_tiles <- tile_directions %>%
  count(target_coords) %>%
  filter(n %% 2 == 1) %>%
  select(black_tile_coord = target_coords)


do_step <- function(black_tiles) {
  black_tiles %>%
    crossing(enframe(coord_diffs, "direction", "coord_diff")) %>%
    rowwise() %>%
    mutate(neighbor_coord = list(list(
      y = black_tile_coord$y + coord_diff$y_diff,
      z = black_tile_coord$z + coord_diff$z_diff
    ))) %>%
    left_join(mutate(black_tiles, is_black = TRUE), by = c('neighbor_coord' = 'black_tile_coord')) %>%
    replace_na(list(is_black = FALSE)) %>%
    group_by(neighbor_coord, is_black) %>%
    summarize(n_black_neighbors = n()) %>%
    mutate(will_be_black = case_when(
      is_black && n_black_neighbors >= 1 && n_black_neighbors <= 2 ~ TRUE,
      !is_black && n_black_neighbors == 2 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    filter(will_be_black) %>%
    ungroup() %>%
    select(black_tile_coord = neighbor_coord)
}

for (idx in 1:100) {
  print(idx)
  black_tiles <- do_step(black_tiles)
}

black_tiles