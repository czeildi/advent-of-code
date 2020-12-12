library(tidyverse)

input <- read_lines("solutions/day11_input.txt") %>%
  tibble(x = .) %>%
  mutate(row_index = seq_len(nrow(.))) %>%
  separate_rows(x, sep = "") %>%
  filter(x != "") %>%
  mutate(x = if_else(x == ".", NA_integer_, 1L)) %>%
  group_by(row_index) %>%
  mutate(col_index = 1:n()) %>%
  ungroup() %>%
  mutate(left_diag = row_index - col_index) %>%
  mutate(right_diag = col_index + row_index)

do_step <- function(input) {
  input %>%
    mutate(n_n = 0L) %>%
    group_by(row_index) %>%
    arrange(col_index) %>%
    mutate(n_n = n_n + coalesce(lead(x), 0L) + coalesce(lag(x), 0L)) %>%
    ungroup() %>%
    group_by(col_index) %>%
    arrange(row_index) %>%
    mutate(n_n = n_n + coalesce(lead(x), 0L) + coalesce(lag(x), 0L)) %>%
    ungroup() %>%
    group_by(left_diag) %>%
    arrange(col_index) %>%
    mutate(n_n = n_n + coalesce(lead(x), 0L) + coalesce(lag(x), 0L)) %>%
    ungroup() %>%
    group_by(right_diag) %>%
    arrange(col_index) %>%
    mutate(n_n = n_n + coalesce(lead(x), 0L) + coalesce(lag(x), 0L)) %>%
    ungroup() %>%
    mutate(new_seat = case_when(
      is.na(x) ~ x,
      n_n >= 4 & x == 1L ~ 0L,
      n_n == 0 & x == 0L ~ 1L,
      TRUE ~ x
    )) %>%
    arrange(row_index, col_index)
}

view_seating <- function(seating) {
  after_step %>%
    select(row_index, col_index, new_seat) %>%
    pivot_wider(id_cols = row_index, names_from = col_index, values_from = new_seat) %>%
    select(-row_index)
}

after_step <- do_step(input)

while(!isTRUE(all.equal(after_step$x, after_step$new_seat))) {
  print('doing another step...')
  print(Sys.time())
  after_step <<- do_step(after_step %>% mutate(x = new_seat))
}

sum(after_step$x, na.rm = TRUE)


# part 2 ------------------------------------------------------------------

library(slider)

do_step <- function(input) {
  input %>%
    mutate(n_n = 0L) %>%
    group_by(row_index) %>%
    arrange(col_index) %>%
    mutate(
      n_n = n_n + 
           slider::slide_int(x, ~last(na.omit(head(.x, -1)), default = 0), .before = Inf) + 
           slider::slide_int(x, ~first(na.omit(tail(.x, -1)), default = 0), .before = 0, .after = Inf)
    ) %>%
    ungroup() %>%
    group_by(col_index) %>%
    arrange(row_index) %>%
    mutate(
      n_n = n_n + 
        slider::slide_int(x, ~last(na.omit(head(.x, -1)), default = 0), .before = Inf) + 
        slider::slide_int(x, ~first(na.omit(tail(.x, -1)), default = 0), .before = 0, .after = Inf)
    ) %>%
    ungroup() %>%
    group_by(left_diag) %>%
    arrange(col_index) %>%
    mutate(
      n_n = n_n + 
        slider::slide_int(x, ~last(na.omit(head(.x, -1)), default = 0), .before = Inf) + 
        slider::slide_int(x, ~first(na.omit(tail(.x, -1)), default = 0), .before = 0, .after = Inf)
    ) %>%
    ungroup() %>%
    group_by(right_diag) %>%
    arrange(col_index) %>%
    mutate(
      n_n = n_n + 
        slider::slide_int(x, ~last(na.omit(head(.x, -1)), default = 0), .before = Inf) + 
        slider::slide_int(x, ~first(na.omit(tail(.x, -1)), default = 0), .before = 0, .after = Inf)
    ) %>%
    ungroup() %>%
    mutate(new_seat = case_when(
      is.na(x) ~ x,
      n_n >= 5 & x == 1L ~ 0L,
      n_n == 0 & x == 0L ~ 1L,
      TRUE ~ x
    )) %>%
    arrange(row_index, col_index)
}

after_step <- do_step(input)

while(!isTRUE(all.equal(after_step$x, after_step$new_seat))) {
  print('doing another step...')
  print(Sys.time())
  after_step <<- do_step(after_step %>% mutate(x = new_seat))
}

sum(after_step$x, na.rm = TRUE)

