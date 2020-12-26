library(tidyverse)

input <- read_file("solutions_2020/day14_input.txt")

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

binary_to_decimal <- function(binary) {
  str_split(binary, '') %>%
    .[[1]] %>%
    as.integer() %>%
    `*`(rev(2^(0:35))) %>%
    sum()
}


# part 1 ------------------------------------------------------------------

mask_value <- function(mask, value) {
  coalesce(
    str_split(mask, '') %>% .[[1]] %>% na_if('X'),
    str_split(value, '') %>% .[[1]]
  ) %>% str_c(collapse = '')
}

s <- program %>%
  rowwise() %>%
  mutate(binary_value = decimal_to_binary(value)) %>%
  mutate(masked_value = mask_value(mask, binary_value)) %>%
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


# part 2 ------------------------------------------------------------------

mask_value <- function(mask, value) {
  coalesce(
    str_split(mask, '') %>% .[[1]] %>% na_if('0'),
    str_split(value, '') %>% .[[1]]
  ) %>% str_c(collapse = '')
}

program_steps <- program %>%
  rowwise() %>%
  mutate(memory_address = mask_value(mask, decimal_to_binary(place))) %>%
  select(-mask) %>%
  mutate(n_floating_bit = str_count(memory_address, 'X'))

expand_address <- function(address) {
  print('expanding...')
  expanded <- address %>%
    str_split('') %>%
    .[[1]] %>%
    c('', .) %>%
    reduce(function(prev, current) {
    current_options <- current
    if (current == 'X') {
      current_options <- c('0', '1')
    }
    crossing(prev, current_options) %>%
      mutate(new = paste0(prev, current_options)) %>%
      pull(new)
  })
  tibble(memory_address = address, actual_address = expanded)
}

sum(2^program_steps$n_floating_bit)

expanded <- map_dfr(program_steps$memory_address, expand_address) %>% distinct()

res <- program_steps %>%
  left_join(unique(expanded)) %>%
  mutate(actual_address_decimal = binary_to_decimal(actual_address)) %>%
  ungroup() %>%
  mutate(row_n = 1:n()) %>%
  arrange(desc(row_n)) %>%
  group_by(actual_address) %>%
  mutate(nn = 1:n()) %>%
  filter(nn == 1) %>%
  pull(value) %>%
  sum()

sprintf('%.0f', res)
