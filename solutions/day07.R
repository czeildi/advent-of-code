library(tidyverse)

rules <- tibble(rule = readr::read_lines('solutions/day07_input.txt')) %>%
  mutate(rule = str_replace_all(rule, ' bags\\.?| bag\\.?', '')) %>%
  separate(rule, c('parent', 'children'), sep = " contain ") %>%
  separate_rows(children, sep = ", ") %>%
  separate(children, into = c('num', 'child_color'), sep = ' ', extra = 'merge')

rules %>%
  count(parent)

rules %>%
  count(child_color)

rules %>% filter(parent == child_color)


# part 1 ------------------------------------------------------------------

rules_at_level <- rules %>%
  filter(num != 'no') %>%
  select(parent, child_color) %>%
  nest_by(parent) %>%
  rowwise() %>%
  mutate(can_contain = ('shiny gold' %in% data$child_color))

can_contain_next_level <- function(rules, can_contain, colors) {
  if (can_contain == TRUE) return(TRUE)
  map_lgl(colors, ~{
    if (! .x %in% pull(rules, parent)) return (FALSE)
    rules %>% filter(parent == .x) %>% pull(can_contain)
  }) %>%
    any()
}

walk(seq_len(nrow(rules_at_level)), ~{
  print(.x)
  rules_at_level <<- rules_at_level %>%
    rowwise() %>%
    mutate(can_contain = can_contain_next_level(rules_at_level, can_contain, data$child_color))
})


# part 2 ------------------------------------------------------------------

initial_rules <- rules %>%
  select(parent, num, child_color) %>%
  mutate(num_i = if_else(num == 'no', 0, as.numeric(num))) %>%
  select(-num) %>%
  mutate(num_bag_in_child = if_else(num_i == 0, 0, NA_real_)) 

rules_at_level <- initial_rules %>%
  group_by(parent) %>%
  mutate(num_total_bag_inside = sum(num_i * num_bag_in_child + num_i))

walk(seq_len(20), ~{
  known_colors <- rules_at_level %>%
    group_by(parent) %>%
    mutate(num_total_bag_inside = sum(num_i * num_bag_in_child + num_i)) %>%
    filter(!is.na(num_total_bag_inside)) %>%
    select(parent, num_total_bag_inside) %>%
    distinct() %>%
    rename(num_bag_in_child = num_total_bag_inside)
  
  if ('shiny gold' %in% known_colors$parent) {
    print(known_colors %>% filter(parent == 'shiny gold') %>% pull(num_bag_in_child))
  }
  
  rules_at_level <<- rbind(
    rules_at_level %>% filter(!is.na(num_bag_in_child)),
    rules_at_level %>% filter(is.na(num_bag_in_child)) %>% select(-num_bag_in_child) %>% left_join(known_colors, by = c("child_color" = "parent"))
  ) %>%
    group_by(parent) %>%
    mutate(num_total_bag_inside = sum(num_i * num_bag_in_child + num_i))
})
