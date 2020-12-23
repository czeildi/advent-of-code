library(tidyverse)
library(profvis)

input <- "853192647"
test_input <- "389125467"
original_nums <- as.integer(str_split(test_input, "")[[1]])
max_cup_value <- 10000
n_round <- 100000

cup_values <- c(original_nums, 10:max_cup_value)
cups <- tibble(cup = cup_values) %>% 
  mutate(
    left_cup = lag(cup, default = max_cup_value), 
    right_cup = lead(cup, default = cup_values[1])
  ) %>%
  rowwise() %>%
  mutate(neighbors = list(list(left_cup = left_cup, cup = cup, right_cup = right_cup))) %>%
  arrange(cup) %>%
  select(cup, neighbors) %>%
  deframe()

profvis({
current_cup <- cup_values[1]

step_destination_cup <- function(cup_label, picked_cups) {
  dest <- cup_label - 1
  if (dest == 0) dest <- max_cup_value
  if (!dest %in% picked_cups) return(dest)
  else return(step_destination_cup(dest, picked_cups))
}

do_one_round <- function(current_cup) {
  neighbor_1 <- cups[[current_cup]]$right_cup
  neighbor_2 <- cups[[neighbor_1]]$right_cup
  neighbor_3 <- cups[[neighbor_2]]$right_cup
  neighbor_4 <- cups[[neighbor_3]]$right_cup
  picked_cups <- c(neighbor_1, neighbor_2, neighbor_3)
  
  destination_cup <- step_destination_cup(current_cup, picked_cups)
  right_neighbor_of_destination_cup <- cups[[destination_cup]]$right_cup
  
  cups[[neighbor_4]]$left_cup <- current_cup
  cups[[current_cup]]$right_cup <- neighbor_4
  
  cups[[destination_cup]]$right_cup <- neighbor_1
  cups[[neighbor_1]]$left_cup <- destination_cup
  cups[[neighbor_3]]$right_cup <- right_neighbor_of_destination_cup
  cups[[right_neighbor_of_destination_cup]]$left_cup <- neighbor_3
}

  for (round_idx in 1:n_round) {
    if (round_idx %% (n_round / 10) == 0) print(round_idx)
    
    do_one_round(current_cup)
    current_cup <- cups[[current_cup]]$right_cup
  }
})

cups[[1]]$right_cup * cups[[cups[[1]]$right_cup]]$cup

# 10.000 100.000 12sec
# 100.000 10.000 12sec
# 10.000 1.000.000 127sec