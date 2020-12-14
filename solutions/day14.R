library(tidyverse)

input <- read_file("solutions/day14_input.txt")

blocks <- input %>% str_split("mask = ") %>% .[[1]] %>% .[. != '']
  

program <- map_dfr(blocks, ~{
  block <- .x %>% str_split('\n') %>% .[[1]] %>% .[. != '' ]
  
  memory_cells <- tibble(x = tail(block, -1)) %>%
    separate(x, c('mem', 'place', 'value'), convert = TRUE) %>%
    select(-mem) %>%
    mutate(mask = block[1])
})


decimal_to_binary <- function(num) {
  result <- num %% 2
  num <- (num - result) / 2
  while (num > 0) {
    result <- paste0(num %% 2, result)
    num <- (num - num %% 2) / 2
  }
  str_pad(result, 36, pad = '0')
}

mask_value <- function(mask, value) {
  coalesce(
    str_split(mask, '') %>% .[[1]] %>% na_if('X'),
    str_split(value, '') %>% .[[1]]
  ) %>% str_c(collapse = '')
}

binary_to_decimal <- function(binary) {
  str_split(binary, '') %>%
    .[[1]] %>%
    as.integer() %>%
    `*`(rev(2^(0:35))) %>%
    sum()
}

s <- program %>%
  rowwise() %>%
  mutate(decimal_value = decimal_to_binary(value)) %>%
  mutate(masked_value = mask_value(mask, decimal_value)) %>%
  mutate(new_value = binary_to_decimal(masked_value)) %>%
  select(place, new_value) %>%
  ungroup() %>%
  mutate(row_n = 1:n()) %>%
  arrange(desc(row_n)) %>%
  group_by(place) %>%
  mutate(nn = 1:n()) %>%
  filter(nn == 1) %>%
  pull(new_value) %>%
  sum()

sprintf('%.0f', s)
