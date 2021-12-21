library(tidyverse)

input <- c(8, 3)
# input <- c(4, 8) # sample

places <- input
scores <- c(0, 0)
die_roll <- 0

die_value <- function(die_roll) {
  if_else(die_roll %% 100 == 0, 100, die_roll %% 100) 
}

place_value <- function(place_idx) {
  if_else(place_idx %% 10 == 0, 10, place_idx %% 10) 
}

while(max(scores) < 1000) {
  # player 1
  die_sum <- sum(die_value(die_roll + 1:3))
  die_roll <- die_roll + 3
  scores[1] <- scores[1] + place_value(places[1] + die_sum)
  places[1] <- place_value(places[1] + die_sum)
  if (max(scores) < 1000) {
    # player 2
    die_sum <- sum(die_value(die_roll + 1:3))
    die_roll <- die_roll + 3
    scores[2] <- scores[2] + place_value(places[2] + die_sum)
    places[2] <- place_value(places[2] + die_sum)
  }
}

min(scores) * die_roll