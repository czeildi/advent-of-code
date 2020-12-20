library(tidyverse)

rules <- tibble(rule = readr::read_lines('solutions/day07_input.txt')) %>%
  mutate(rule = str_replace_all(rule, ' bags\\.?| bag\\.?', '')) %>%
  separate(rule, c('parent', 'children'), sep = " contain ") %>%
  separate_rows(children, sep = ", ") %>%
  extract(children, into = c('num', 'child_color'), regex = "(no|\\d+) ([a-z ]+)")

assertthat::assert_that(nrow(filter(rules, parent == child_color)) == 0)

# part 1 ------------------------------------------------------------------

parent_children_relationships <- rules %>%
  filter(num != 'no') %>%
  group_by(parent) %>%
  summarize(child_colors = list(child_color)) %>%
  deframe()

can_contain_color <- function(parent_color, descendant_color = 'shiny gold') {
  if (parent_color == descendant_color) return(TRUE)
  direct_children <- parent_children_relationships[[parent_color]]
  map_lgl(direct_children, m_can_contain_color) %>%
    any()
}

m_can_contain_color <- memoise::memoise(can_contain_color)

rules %>%
  filter(parent != 'shiny gold') %>%
  pull(parent) %>%
  unique() %>%
  map_lgl(m_can_contain_color) %>%
  sum()

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
