library(tidyverse)

input <- read_file("solutions_2021/input20.txt") %>% 
  str_split("\n\n") %>% 
  pluck(1)

mapping <- input[1] %>% str_split("\n") %>% pluck(1) %>% paste(collapse = "") %>% 
  str_split("") %>% pluck(1) %>%
  map_int(~ if_else(. == ".", 0L, 1L))

image <- tibble(value = input[2]) %>% 
  separate_rows(value, sep = "\n") %>% 
  mutate(y = 1:n()) %>% 
  separate_rows(value, sep = "") %>% 
  filter(value != "") %>% 
  group_by(y) %>% 
  mutate(x = 1:n()) %>% 
  ungroup() %>% 
  mutate(value = if_else(value == '.', 0L, 1L))

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
) %>% 
  mutate(idx = 1:9)

binary_vec_to_decimal <- function(binary) {
  sum(rev(binary) * 2 ^ (0:(length(binary) - 1)))
}

# for debugging
show_image <- function(image) {
  image %>% 
    ggplot(aes(x = x, y = -y, alpha = value)) + 
    geom_tile()
}

enhance <- function(image, border_value) {
  image %>% 
    inner_join(neighbors, by = character(0)) %>% 
    mutate(nx = x + dx, ny = y + dy) %>% 
    left_join(image, by = c('nx' = 'x', 'ny' = 'y')) %>% 
    rename(value = value.x, nvalue = value.y) %>% 
    mutate(nvalue = if_else(is.na(nvalue), border_value, nvalue)) %>% 
    group_by(x, y) %>% 
    summarize(binary = binary_vec_to_decimal(nvalue), .groups = "drop") %>% 
    mutate(value = mapping[binary + 1]) %>% 
    select(x, y, value)
    
}

add_border <- function(image) {
  crossing(
    x = (min(image$x) - 1L):(max(image$x) + 1L),
    y = (min(image$y) - 1L):(max(image$y) + 1L)
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
