library(tidyverse)

input <- str_split(read_file("solutions_2021/input14.txt"), "\n\n")[[1]]

starting <- input[1]

rules <- tibble(x = input[2]) %>% 
  separate_rows(x, sep = "\n") %>% 
  filter(x != "") %>% 
  extract(x, c("pair", "inserted"), "([A-Z]+) -> ([A-Z])", convert = TRUE) %>% 
  rowwise() %>% 
  mutate(new_pairs = list(c(
    paste0(str_split(pair, "")[[1]][1], inserted),
    paste0(inserted, str_split(pair, "")[[1]][2]))
  )) %>% 
  ungroup() %>% 
  select(-inserted)


pairs_from_string <- function(text) {
  text_chars <- str_split(text, "")[[1]]
  head(paste0(text_chars, lead(text_chars)), -1)
}

initial_pair_frequencies <- table(pairs_from_string(starting)) %>% 
  enframe(name = "pair", value = 'freq') %>% 
  mutate(freq = as.numeric(freq))

final_pair_frequencies <- reduce(1:40, function(prev, current) {
  prev %>% 
    inner_join(rules) %>% 
    unnest_longer(new_pairs) %>% 
    group_by(new_pairs) %>% 
    summarize(freq = sum(freq)) %>% 
    rename(pair = new_pairs)
}, .init = initial_pair_frequencies)

final_letter_frequencies <- final_pair_frequencies %>% 
  rowwise() %>% 
  mutate(letters = list(str_split(pair, "")[[1]])) %>% 
  unnest_longer(letters) %>% 
  group_by(letters) %>% 
  summarize(freq = ceiling(sum(freq)/2)) %>% 
  arrange(freq)

options(scipen=99)
tail(final_letter_frequencies$freq, 1) - head(final_letter_frequencies$freq, 1)
