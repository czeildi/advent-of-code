library(tidyverse)

num_cycle <- 6

input <- read_lines("solutions_2020/day17_input.txt") %>%
  tibble(cell = .) %>%
  mutate(x = seq_len(nrow(.))) %>%
  separate_rows(cell, sep = "") %>%
  filter(cell != "") %>%
  mutate(cell = if_else(cell == ".", 0L, 1L)) %>%
  group_by(x) %>%
  mutate(y = 1:n()) %>%
  ungroup() %>%
  mutate(z = 0, w = 0)

max_input_x <- max(input$x)
max_input_y <- max(input$y)

coordinates_in_world <- crossing(
  x = (-num_cycle):(max_input_x + 1 + num_cycle),
  y = (-num_cycle):(max_input_y + 1 + num_cycle),
  z = (-num_cycle):num_cycle,
  w = (-num_cycle):num_cycle
)

world <- input %>%
  right_join(coordinates_in_world) %>%
  replace_na(list(cell = 0))

by_x_directions <- crossing(v_x = 1, v_y = -1:1, v_z = -1:1, v_w = -1:1)
by_y_directions <- crossing(v_x = 0, v_y = 1, v_z = -1:1, v_w = -1:1)
by_z_directions <- crossing(v_x = 0, v_y = 0, v_z = 1, v_w = -1:1)
by_w_directions <- crossing(v_x = 0, v_y = 0, v_z = 0, v_w = 1)

n_neighbors_by_x_direction <- function(world, v_x, v_y, v_z, v_w) {
  world %>%
    rowwise() %>%
    mutate(direction_const = list(c(y - x/v_x * v_y, z - x/v_x * v_z, w - x/v_x * v_w))) %>%
    ungroup() %>%
    group_by(direction_const) %>%
    arrange(x) %>%
    mutate(n_neighbor = n_neighbor + coalesce(lead(cell), 0L) + coalesce(lag(cell), 0L)) %>%
    ungroup()
}

n_neighbors_by_y_direction <- function(world, v_x, v_y, v_z, v_w) {
  world %>%
    rowwise() %>%
    mutate(direction_const = list(c(x - y/v_y * v_x, z - y/v_y * v_z, w - y/v_y * v_w))) %>%
    ungroup() %>%
    group_by(direction_const) %>%
    arrange(y) %>%
    mutate(n_neighbor = n_neighbor + coalesce(lead(cell), 0L) + coalesce(lag(cell), 0L)) %>%
    ungroup()
}

n_neighbors_by_z_direction <- function(world, v_x, v_y, v_z, v_w) {
  world %>%
    rowwise() %>%
    mutate(direction_const = list(c(x - z/v_z * v_x, y - z/v_z * v_y, w - z/v_z * v_w))) %>%
    ungroup() %>%
    group_by(direction_const) %>%
    arrange(z) %>%
    mutate(n_neighbor = n_neighbor + coalesce(lead(cell), 0L) + coalesce(lag(cell), 0L)) %>%
    ungroup()
}

n_neighbors_by_w_direction <- function(world, v_x, v_y, v_z, v_w) {
  world %>%
    rowwise() %>%
    mutate(direction_const = list(c(x, y, z))) %>%
    ungroup() %>%
    group_by(direction_const) %>%
    arrange(w) %>%
    mutate(n_neighbor = n_neighbor + coalesce(lead(cell), 0L) + coalesce(lag(cell), 0L)) %>%
    ungroup()
}

count_neighbors <- function(world) {
  res <- world %>%
    mutate(n_neighbor = 0L)
  pwalk(by_x_directions, function(v_x, v_y, v_z, v_w) {
      res <<- n_neighbors_by_x_direction(res, v_x, v_y, v_z, v_w)
  })
  pwalk(by_y_directions, function(v_x, v_y, v_z, v_w) {
    res <<- n_neighbors_by_y_direction(res, v_x, v_y, v_z, v_w)
  })
  pwalk(by_z_directions, function(v_x, v_y, v_z, v_w) {
    res <<- n_neighbors_by_z_direction(res, v_x, v_y, v_z, v_w)
  })
  pwalk(by_w_directions, function(v_x, v_y, v_z, v_w) {
    res <<- n_neighbors_by_w_direction(res, v_x, v_y, v_z, v_w)
  })
  res
}

do_step <- function(world) {
  world %>%
    count_neighbors() %>%
    mutate(new_cell_value = case_when(
      n_neighbor >= 2 & n_neighbor <= 3 & cell == 1L ~ 1L,
      cell == 1L ~ 0L,
      n_neighbor == 3 & cell == 0L ~ 1L,
      cell == 0L ~ 0L
    ))
}

for (cycle_index in seq_len(num_cycle)) {
  print(cycle_index)
  print(Sys.time())
  world <<- do_step(world) %>%
    mutate(cell = new_cell_value)
}

world %>%
  filter(cell == 1) %>%
  nrow()
