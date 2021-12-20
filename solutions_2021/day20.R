library(tidyverse)

input <- str_split(read_file("solutions_2021/input20.txt"), "\n\n")[[1]]

mapping <- str_split(input[1], "")[[1]] %>% 
  map_int(~ as.integer(. == "#"))

image <- tibble(value = input[2]) %>% 
  separate_rows(value, sep = "\n") %>% 
  mutate(y = 1:n()) %>% 
  separate_rows(value, sep = "") %>% 
  filter(value != "") %>% 
  group_by(y) %>% 
  mutate(x = 1:n()) %>% 
  ungroup() %>% 
  mutate(value = as.integer(value == "#"))

neighbors <- tribble(
  ~direction, ~dx, ~dy,
  "tl", -1, -1,
  "t", 0, -1,
  "tr", 1, -1,
  "l", -1, 0,
  "self", 0, 0,
  "r", 1, 0,
  "bl", -1, 1,
  "b", 0, 1,
  "br", 1, 1
)

binary_vec_to_decimal <- function(binary) {
  sum(rev(binary) * 2 ^ (0:(length(binary) - 1)))
}

enhance <- function(image, border_value) {
  image %>% 
    inner_join(neighbors, by = character(0)) %>% 
    mutate(nb_x = x + dx, nb_y = y + dy) %>% 
    left_join(image, by = c('nb_x' = 'x', 'nb_y' = 'y')) %>% 
    mutate(nb_value = if_else(is.na(value.y), border_value, value.y)) %>% 
    group_by(x, y) %>% 
    summarize(binary = binary_vec_to_decimal(nb_value), .groups = "drop") %>% 
    mutate(value = mapping[binary + 1]) %>% 
    select(x, y, value)
}

add_border <- function(image) {
  crossing(
    x = (min(image$x) - 1):(max(image$x) + 1),
    y = (min(image$y) - 1):(max(image$y) + 1)
  ) %>% 
    left_join(image, by = c('x', 'y'))
}

enhanced <- reduce(1:2, function(prev, current) {
  list(
    image = enhance(add_border(prev$image), prev$border_value),
    border_value = if_else(prev$border_value == 0, head(mapping, 1), tail(mapping, 1))
  )
}, .init = list(image = image, border_value = 0L))

sum(enhanced$image$value)
