library(tidyverse)
library(profvis)

input <- "853192647"
test_input <- "389125467"
original_nums <- as.integer(str_split(test_input, "")[[1]])
max_cup_value <- 10000
n_round <- 1000000

cup_values <- c(original_nums, 10:max_cup_value)
cups <- tibble(cup = cup_values) %>% 
  mutate(
    right_cup = lead(cup, default = cup_values[1])
  ) %>%
  pull(right_cup)

# profvis({
current_cup <- cup_values[1]

step_destination_cup <- function(cup_label, picked_cups) {
  dest <- cup_label - 1
  if (dest == 0) dest <- max_cup_value
  if (!dest %in% picked_cups) return(dest)
  else return(step_destination_cup(dest, picked_cups))
}

do_one_round <- function(current_cup) {
  neighbor_1 <- cups[current_cup]
  neighbor_2 <- cups[neighbor_1]
  neighbor_3 <- cups[neighbor_2]
  neighbor_4 <- cups[neighbor_3]
  picked_cups <- c(neighbor_1, neighbor_2, neighbor_3)
  
  destination_cup <- step_destination_cup(current_cup, picked_cups)
  right_neighbor_of_destination_cup <- cups[destination_cup]
  
  cups[current_cup] <- neighbor_4
  cups[destination_cup] <- neighbor_1
  cups[neighbor_3] <- right_neighbor_of_destination_cup
}

system.time({
  for (round_idx in 1:n_round) {
    if (round_idx %% (n_round / 10) == 0) print(round_idx)
    
    do_one_round(current_cup)
    current_cup <- cups[current_cup]
  }
})

cups[1] * cups[cups[1]]

# 10.000 100.000 1.8sec
# 10.000 1.000.000 17sec