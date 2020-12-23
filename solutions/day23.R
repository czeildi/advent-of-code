library(tidyverse)

input <- "853192647"
test_input <- "389125467"

cups <- input %>% str_split("") %>% .[[1]] %>% as.integer()

step_destination_cup <- function(cup_label, picked_cups) {
  dest <- cup_label - 1
  if (dest == 0) dest <- 9
  if (!dest %in% picked_cups) return(dest)
  else return(step_destination_cup(dest, picked_cups))
}

do_one_round <- function(cups) {
  current_cup <- cups[1]
  picked_cups <- cups[2:4]
  
  destination_cup <- step_destination_cup(current_cup, picked_cups)
  
  destination_cup_index <- which(cups == destination_cup)
  
  if (destination_cup_index == 9) {
    c(cups[5:destination_cup_index], cups[2:4], cups[1])
  } else {
    c(cups[5:destination_cup_index], cups[2:4], cups[(destination_cup_index + 1):9], cups[1])
  }
}

for (round_idx in 1:100) {
  print(round_idx)
  cups <- do_one_round(cups)
  print(str_c(cups, collapse = ","))
}