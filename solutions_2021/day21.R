library(tidyverse)

input <- c(8, 3)
# input <- c(4, 8) # sample

# part 1 -------------------------------------------------------------
winning_score <- 1000
places <- input
scores <- c(0, 0)
die_roll <- 0

one_indexed_remainder <- function(value, mod) {
  if_else(value %% mod == 0, mod, value %% mod) 
}

die_value <- function(die_roll) one_indexed_remainder(die_roll, 100)
place_value <- function(place_idx) one_indexed_remainder(place_idx, 10)

roll_for_player <- function(player_idx) {
  die_sum <- sum(die_value(die_roll + 1:3))
  die_roll <<- die_roll + 3
  scores[player_idx] <<- scores[player_idx] + place_value(places[player_idx] + die_sum)
  places[player_idx] <<- place_value(places[player_idx] + die_sum)
}

while(max(scores) < winning_score) {
  roll_for_player(1)
  if (max(scores) < winning_score) {
    roll_for_player(2)
  }
}

min(scores) * die_roll

# part 2 -----------------------------------------------------------------

# transition matrix
die_sum_frequencies <- cross3(1:3, 1:3, 1:3) %>% 
  map_int(~sum(unlist(.))) %>% 
  table() %>% 
  as.list()

n_possibility_in_single_roll <- function(from, to) {
  score_to_gain <- to - from
  if (score_to_gain < 0) score_to_gain <- score_to_gain + 10
  if (score_to_gain < 3) return(0)
  die_sum_frequencies[[as.character(score_to_gain)]]
}

transitions <- crossing(from = 1:10, to = 1:10) %>% 
  rowwise() %>% 
  mutate(n_possibility = n_possibility_in_single_roll(from, to))

# simulate the game in all universes

winning_score <- 21
winning_universes <- c(0, 0)

roll_die <- function(state) {
  state %>% 
    inner_join(transitions, by = "from") %>% 
    mutate(n = n * n_possibility, score = score + to) %>% 
    filter(n != 0) %>% 
    select(from = to, n, score) %>% 
    group_by(from, score) %>% 
    summarize(n = sum(n), .groups = "drop")
}

universes <- list(
  tibble(from = input[1], n = 1, score = 0),
  tibble(from = input[2], n = 1, score = 0)
)

roll_for_player_p2 <- function(player_idx) {
  after_roll <- roll_die(universes[[player_idx]])
  
  win_for_player <- after_roll %>% 
    filter(score >= winning_score) %>% 
    pull(n) %>% 
    sum()
  
  loses_for_other_player <- sum(universes[[3 - player_idx]]$n)
  winning_universes[player_idx] <<- winning_universes[player_idx] + win_for_player * loses_for_other_player
  
  universes[[player_idx]] <<- after_roll %>% 
    filter(score < winning_score)
}

while(nrow(universes[[1]]) > 0 && nrow(universes[[2]]) > 0) {
  roll_for_player_p2(1)
  if (nrow(universes[[1]]) > 0 && nrow(universes[[2]]) > 0) {
    roll_for_player_p2(2)
  }
}

options(scipen = 99)

max(winning_universes)
