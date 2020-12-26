library(tidyverse)

input <- read_lines("solutions_2020/day11_input.txt") %>%
  tibble(x = .) %>%
  mutate(row = 1:n()) %>%
  separate_rows(x, sep = "") %>%
  filter(x != "") %>%
  mutate(x = if_else(x == ".", NA_integer_, 1L)) %>%
  group_by(row) %>%
  mutate(col = 1:n()) %>%
  ungroup() %>%
  mutate(left_diag = row - col) %>%
  mutate(right_diag = col + row)

add_neighbors_in_direction <- function(seating, ...) {
  seating %>%
    group_by(...) %>%
    mutate(n_n = n_n + coalesce(lead(x), 0L) + coalesce(lag(x), 0L)) %>%
    ungroup()
}

do_step <- function(input) {
  input %>%
    mutate(n_n = 0L) %>%
    arrange(row, col) %>%
    add_neighbors_in_direction(col) %>%
    add_neighbors_in_direction(row) %>%
    add_neighbors_in_direction(left_diag) %>%
    add_neighbors_in_direction(right_diag) %>%
    mutate(new_seat = case_when(
      is.na(x) ~ x,
      n_n >= 4 & x == 1L ~ 0L,
      n_n == 0 & x == 0L ~ 1L,
      TRUE ~ x
    ))
}

# for debug purposes
view_seating <- function(seating) {
  seating %>%
    select(row, col, new_seat) %>%
    pivot_wider(id_cols = row, names_from = col, values_from = new_seat) %>%
    select(-row)
}

after_step <- do_step(input)

while(!isTRUE(all.equal(after_step$x, after_step$new_seat))) {
  print(glue::glue('{Sys.time()}: doing another step...'))
  after_step <<- do_step(after_step %>% mutate(x = new_seat))
}

sum(after_step$x, na.rm = TRUE)


# part 2 ------------------------------------------------------------------

library(slider)

add_visibles_in_direction <- function(seating, ...) {
  seating %>%
    group_by(...) %>%
    mutate(
      n_n = n_n + 
        slide_int(x, ~last(na.omit(head(.x, -1)), default = 0), .before = Inf, .after = 0) + 
        slide_int(x, ~first(na.omit(tail(.x, -1)), default = 0), .before = 0, .after = Inf)
    ) %>%
    ungroup()
}

do_step <- function(input) {
  input %>%
    mutate(n_n = 0L) %>%
    arrange(row, col) %>%
    add_visibles_in_direction(row) %>%
    add_visibles_in_direction(col) %>%
    add_visibles_in_direction(left_diag) %>%
    add_visibles_in_direction(right_diag) %>%
    mutate(new_seat = case_when(
      is.na(x) ~ x,
      n_n >= 5 & x == 1L ~ 0L,
      n_n == 0 & x == 0L ~ 1L,
      TRUE ~ x
    ))
}

after_step <- do_step(input)

while(!isTRUE(all.equal(after_step$x, after_step$new_seat))) {
  print(glue::glue('{Sys.time()}: doing another step...'))
  after_step <<- do_step(after_step %>% mutate(x = new_seat))
}

sum(after_step$x, na.rm = TRUE)

