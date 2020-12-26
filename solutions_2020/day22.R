library(profvis)
library(tidyverse)

input <- tibble(cards = read_file("solutions_2020/day22_input.txt")) %>%
  separate_rows(cards, sep = "\n\n") %>%
  mutate(player_idx = 1:n()) %>%
  mutate(cards = str_replace_all(cards, "Player \\d:", "")) %>%
  separate_rows(cards, sep = "\n", convert = TRUE) %>%
  filter(cards != "")

cards <- list(
  '1' = filter(input, player_idx == 1)$cards,
  '2' = filter(input, player_idx == 2)$cards
)
# global variable to keep track of repeated dealings
seen_card_dealings <- list()


play_game <- function(cards, game_depth) {
  seen_card_dealings[[as.character(game_depth)]] <- character(0)
  round_id <- 1
  
  should_proceed <- should_game_proceed(cards, game_depth, round_id)
  while (should_proceed$should_proceed) {
    winner_id <- round_winner(cards, game_depth)
    cards <- move_drawn_cards_to_winner_deck(cards, winner_id)
    
    round_id <- round_id + 1
    should_proceed <- should_game_proceed(cards, game_depth, round_id)
  }
  
  if (should_proceed$end_game_type == 'instant_end') winner_id <- 1
  
  return(list(winner_id = winner_id, cards = cards))
}

should_game_proceed <- function(cards, game_depth, round_id) {
  card_dealing_id <- digest::digest(cards)
  if (card_dealing_id %in% seen_card_dealings[[as.character(game_depth)]]) {
    return(list(should_proceed = FALSE, end_game_type = "instant_end"))
  }
  seen_card_dealings[[as.character(game_depth)]][round_id] <<- card_dealing_id
  
  list(
    should_proceed = length(cards[[1]]) > 0 && length(cards[[2]]),
    end_game_type = "no_cards_left"
  )
}

round_winner <- function(cards, game_depth) {
  if (!should_play_subgame(cards)) return(player_id_with_top_card(cards))

  cards_for_subgame <- map(cards, ~tail(., -1)[1:(.[1])])
  subgame_result <- play_game(cards_for_subgame, game_depth + 1)
  subgame_result$winner_id
}

should_play_subgame <- function(cards) {
  cards[[1]][1] <= length(cards[[1]]) - 1 && cards[[2]][1] <= length(cards[[2]]) - 1
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

end_situation <- play_game(cards, 1)

winner_deck <- end_situation$cards[[end_situation$winner_id]]
sum(winner_deck * rev(seq_along(winner_deck)))
