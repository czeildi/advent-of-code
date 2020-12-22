library(tidyverse)
library(profvis)

input <- tibble(x = read_file("day22_input.txt")) %>%
  separate_rows(x, sep = "\n\n") %>%
  filter(x != "") %>%
  mutate(player_idx = 1:n()) %>%
  mutate(x = str_replace_all(x, "Player \\d:", "")) %>%
  separate_rows(x, sep = "\n", convert = TRUE) %>%
  filter(x != "")

cards <- group_by(input, player_idx) %>% summarize(card_values = list(x)) %>% deframe()
seen_card_dealings <- map(1:20, ~character(1000))

# profvis({
should_game_proceed <- function(cards, game_id, step_id_in_game) {
  card_dealing_id <- digest::digest(cards)
  if (card_dealing_id %in% seen_card_dealings[[game_id]]) {
    print("ending due to repeated dealing")
    return(list(value = FALSE, type = "instand_end"))
  }
  
  have_cards <- length(cards[[1]]) > 0 && length(cards[[2]]) > 0
  
  seen_card_dealings[[game_id]][step_id_in_game] <<- card_dealing_id
  return(list(value = have_cards, type = "no_cards_left"))
}

should_play_subgame <- function(cards) {
  map_lgl(cards, ~.[1] <= (length(.) - 1)) %>% all()
}

player_id_with_top_card <- function(cards) {
  if (cards[[1]][1] > cards[[2]][1]) return(1)
  return(2)
}

apply_winner <- function(cards, winner_id) {
  if (winner_id == 1) {
    return(list(
      c(tail(cards[[1]], -1),cards[[1]][1], cards[[2]][1]),
      tail(cards[[2]], -1)
    ))
  } else {
    return(list(
      tail(cards[[1]], -1),
      c(tail(cards[[2]], -1),cards[[2]][1], cards[[1]][1])
    ))
  }
}

play_game <- function(cards, game_id) {
  print(game_id)
  current_cards <- cards
  step_id_in_game <- 1
  seen_card_dealings[[game_id]] <- character(1000)
  
  should_proceed <- should_game_proceed(current_cards, game_id, step_id_in_game)
  while (should_proceed[['value']]) {
    # print(glue::glue("Game {game_id} round {step_id_in_game}"))
    if(should_play_subgame(current_cards)) {
      cards_for_subgame <- map(current_cards, ~{
        tail(., -1)[1:(.[1])]
      })
      res <- m_play_game(cards_for_subgame, game_id + 1)
      subgame_winner_id <- res$winner_id
    } else {
      subgame_winner_id <- player_id_with_top_card(current_cards)
    } 
    current_cards <- apply_winner(current_cards, subgame_winner_id)
    
    step_id_in_game <- step_id_in_game + 1
    should_proceed <- should_game_proceed(current_cards, game_id, step_id_in_game)
  }
  if (should_proceed[['type']] == 'instand_end') {
    list(
      winner_id = 1,
      cards = current_cards
    )
  } else {
    list(
      winner_id = subgame_winner_id,
      cards = current_cards
    )
  }
}

m_play_game <- memoise::memoise(play_game)

end_situation <- m_play_game(cards, 1)

winner_deck <- end_situation$cards[[end_situation$winner_id]]
print(sum(winner_deck * rev(seq_along(winner_deck))))

# })