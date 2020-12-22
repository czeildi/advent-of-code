library(profvis)
library(tidyverse)

input <- tibble(x = read_file("solutions/day22_input.txt")) %>%
  separate_rows(x, sep = "\n\n") %>%
  mutate(player_idx = 1:n()) %>%
  mutate(x = str_replace_all(x, "Player \\d:", "")) %>%
  separate_rows(x, sep = "\n", convert = TRUE) %>%
  filter(x != "")

cards <- group_by(input, player_idx) %>% summarize(card_values = list(x)) %>% deframe()

# global variable to keep track of repeated dealings
# 20 is a guess at max game depth
seen_card_dealings <- map(1:20, ~character(0))

should_game_proceed <- function(cards, game_id, step_id_in_game) {
  card_dealing_id <- digest::digest(cards)
  if (card_dealing_id %in% seen_card_dealings[[game_id]]) {
    # print("ending due to repeated dealing")
    return(list(should_proceed = FALSE, end_game_type = "instant_end"))
  }
  seen_card_dealings[[game_id]][step_id_in_game] <<- card_dealing_id
  
  list(
    should_proceed = length(cards[[1]]) > 0 && length(cards[[2]]),
    end_game_type = "no_cards_left"
  )
}

should_play_subgame <- function(cards) {
  cards[[1]][1] <= length(cards[[1]]) - 1 && cards[[2]][1] <= length(cards[[2]]) - 1
}

cards_for_subgame <- function(cards) {
  map(cards, ~tail(., -1)[1:(.[1])])
}

player_id_with_top_card <- function(cards) { 
  if (cards[[1]][1] > cards[[2]][1]) return(1)
  return(2)
}
  

move_drawn_cards_to_winner_deck <- function(cards, winner_id) {
  if (winner_id == 1) {
    return(list(
      '1' = c(tail(cards[[1]], -1), cards[[1]][1], cards[[2]][1]),
      '2' = tail(cards[[2]], -1)
    ))
  } else {
    return(list(
      '1' = tail(cards[[1]], -1),
      '2' = c(tail(cards[[2]], -1), cards[[2]][1], cards[[1]][1])
    ))
  }
}

play_game <- function(cards, game_id) {
  print(glue::glue("Game {game_id} level deep"))
  
  step_id_in_game <- 1
  seen_card_dealings[[game_id]] <- character(0)
  
  should_proceed <- should_game_proceed(cards, game_id, step_id_in_game)
  while (should_proceed$should_proceed) {
    # print(glue::glue("Game {game_id} level deep, round {step_id_in_game}"))
    
    if(should_play_subgame(cards)) {
      winner_id <- play_game(cards_for_subgame(cards), game_id + 1)$winner_id
    } else {
      winner_id <- player_id_with_top_card(cards)
    } 
    cards <- move_drawn_cards_to_winner_deck(cards, winner_id)
    
    step_id_in_game <- step_id_in_game + 1
    should_proceed <- should_game_proceed(cards, game_id, step_id_in_game)
  }
  
  if (should_proceed$end_game_type == 'instant_end') winner_id <- 1
  
  return(list(winner_id = winner_id, cards = cards))
}

system.time({
  end_situation <- play_game(cards, 1)
})

winner_deck <- end_situation$cards[[end_situation$winner_id]]
sum(winner_deck * rev(seq_along(winner_deck)))
