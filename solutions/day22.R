library(tidyverse)

input <- tibble(x = read_file("solutions/day22_input.txt")) %>%
  separate_rows(x, sep = "\n\n") %>%
  filter(x != "") %>%
  mutate(player_idx = 1:n()) %>%
  mutate(x = str_replace_all(x, "Player \\d:", "")) %>%
  separate_rows(x, sep = "\n", convert = TRUE) %>%
  filter(x != "") %>%
  group_by(player_idx) %>%
  mutate(deck_idx = 1:n()) %>% 
  ungroup() %>%
  rename(card_value = x)

cards <- input %>% nest(data = c(card_value, deck_idx))

should_game_proceed <- function(cards) {
  cards %>%
    rowwise() %>%
    mutate(has_cards_left = nrow(data) > 0) %>%
    ungroup() %>%
    pull(has_cards_left) %>%
    all()
}

step_cards <- function(cards, is_winner, played_cards) {
  if (!is_winner) {
    cards %>%
      filter(deck_idx != 1) %>%
      mutate(deck_idx = seq_len(n()))
  } else {
    cards %>%
      filter(deck_idx != 1) %>%
      rbind(tibble(deck_idx = 1, card_value = c(max(played_cards), min(played_cards)))) %>%
      mutate(deck_idx = seq_len(n()))
  }
}

step_idx <- 1

while (should_game_proceed(cards)) {
  print(step_idx)
  cards <<- cards %>%
    rowwise() %>%
    mutate(top_card = filter(data, deck_idx == 1) %>% pull(card_value)) %>%
    ungroup () %>%
    mutate(played_cards = list(top_card)) %>%
    rowwise() %>%
    mutate(is_winner = (top_card == max(played_cards))) %>%
    mutate(data = list(step_cards(data, is_winner, played_cards))) %>%
    ungroup()
  step_idx <- step_idx + 1
}

reduce(cards$data, rbind) %>%
  ungroup() %>%
  arrange(desc(deck_idx)) %>%
  mutate(multiplier = 1:n()) %>%
  summarize(s = sum(card_value * multiplier))
