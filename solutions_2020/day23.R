library(tidyverse)

input <- "853192647" # test input: "389125467"
original_nums <- as.integer(str_split(input, "")[[1]])
max_cup_value <- 1000000
n_round <- 10000000

cups_cw_neighbor <- tibble(cup = c(original_nums, seq(10, length.out = max_cup_value - 10 + 1))) %>% 
  mutate(right_cup = lead(cup, default = original_nums[1])) %>%
  arrange(cup) %>%
  pull(right_cup)

get_destination_cup <- function(current_cup, picked_cups) {
  dest <- current_cup - 1
  if (dest == 0) dest <- max_cup_value
  
  if (dest == picked_cups[1] || dest == picked_cups[2] || dest == picked_cups[3]) {
    return(get_destination_cup(dest, picked_cups))
  }
  return(dest)
}

do_one_round <- function(current_cup) {
  cw_neighbor_1 <- cups_cw_neighbor[current_cup]
  cw_neighbor_2 <- cups_cw_neighbor[cw_neighbor_1]
  cw_neighbor_3 <- cups_cw_neighbor[cw_neighbor_2]
  cw_neighbor_4 <- cups_cw_neighbor[cw_neighbor_3]
  
  destination_cup <- get_destination_cup(
    current_cup,
    picked_cups = c(cw_neighbor_1, cw_neighbor_2, cw_neighbor_3)
  )
  cw_neighbor_1_of_destination_cup <- cups_cw_neighbor[destination_cup]
  
  
  cups_cw_neighbor[current_cup] <<- cw_neighbor_4
  cups_cw_neighbor[destination_cup] <<- cw_neighbor_1
  cups_cw_neighbor[cw_neighbor_3] <<- cw_neighbor_1_of_destination_cup
  
  cw_neighbor_4
}

current_cup <- original_nums[1]

for (round_idx in 1:n_round) {
  current_cup <- do_one_round(current_cup)
}

cups_cw_neighbor[1] * cups_cw_neighbor[cups_cw_neighbor[1]]
