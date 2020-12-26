library(tidyverse)

input <- read_file("solutions_2020/day16_input.txt") %>%
  str_split("\n\n") %>% .[[1]]

rules <- tibble(x = input[1]) %>%
  separate_rows(x, sep = '\n') %>%
  extract(x, c('field', '1', '2'), regex = "^([a-z ]+): ([0-9]+-[0-9]+) or ([0-9]+-[0-9]+)$") %>%
  pivot_longer(c(`1`, `2`), names_to = 'interval_n', values_to = 'interval') %>%
  separate(interval, c('lower_limit', 'upper_limit'), convert = TRUE)

nearby_tickets <- tibble(num = str_replace(input[3], 'nearby tickets:\n', '')) %>%
  separate_rows(num, sep = '\n') %>%
  filter(num != '') %>%
  mutate(ticket_idx = 1:n()) %>%
  separate_rows(num, sep = ',', convert = TRUE) %>%
  group_by(ticket_idx) %>%
  mutate(place_in_ticket = 1:n()) %>%
  ungroup()


# part 1 ------------------------------------------------------------------

allowed_numbers <- rules %>%
  rowwise() %>%
  mutate(allowed_numbers = list(lower_limit:upper_limit)) %>%
  pull(allowed_numbers) %>%
  unlist() %>%
  unique() %>%
  sort()

nearby_tickets %>%
  anti_join(tibble(num = allowed_numbers)) %>%
  summarize(sum_of_invalid = sum(num))


# part 2 ------------------------------------------------------------------

invalid_ticket_ids <- nearby_tickets %>%
  anti_join(tibble(num = allowed_numbers)) %>%
  select(ticket_idx) %>%
  distinct()

valid_tickets <- nearby_tickets %>%
  anti_join(invalid_ticket_ids)

allowed_by_field <- rules %>%
  rowwise() %>%
  mutate(allowed_numbers = list(lower_limit:upper_limit)) %>%
  ungroup() %>%
  group_by(field) %>%
  summarize(allowed_numbers = list(unique(unlist(allowed_numbers))))

options_by_field <- valid_tickets %>%
  group_by(place_in_ticket) %>%
  summarise(nums_in_place = list(unique(num))) %>%
  crossing(field = allowed_by_field$field) %>%
  inner_join(allowed_by_field) %>%
  rowwise() %>%
  mutate(is_possible = all(nums_in_place %in% allowed_numbers)) %>%
  filter(is_possible) %>%
  select(place_in_ticket, field) %>%
  ungroup()

known_fields <- tibble(place_in_ticket = integer(0), field = character(0))

while(nrow(known_fields) < nrow(allowed_by_field)) {
  newly_known_fields <- options_by_field %>%
    anti_join(tibble(field = known_fields$field)) %>%
    group_by(place_in_ticket) %>%
    mutate(n_option = n()) %>%
    filter(n_option == 1) %>%
    select(place_in_ticket, field)
  known_fields <<- rbind(known_fields, newly_known_fields)
}

known_fields

my_ticket <- tibble(num = str_replace(input[2], 'your ticket:\n', '')) %>%
  separate_rows(num, sep = ',', convert = TRUE) %>%
  mutate(place_in_ticket = 1:n()) %>%
  inner_join(known_fields) 

my_ticket %>%
  filter(str_detect(field, '^departure')) %>%
  pull(num) %>%
  as.numeric() %>%
  reduce(`*`) %>%
  sprintf('%.0f', .)
