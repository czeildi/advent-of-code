library(tidyverse)

input <- str_split(read_file("solutions_2021/input14.txt"), "\n\n")[[1]]

starting <- input[1]

# part 1

rules <- tibble(x = input[2]) %>% 
  separate_rows(x, sep = "\n") %>% 
  filter(x != "") %>% 
  extract(x, c("pair", "inserted"), "([A-Z]+) -> ([A-Z])", convert = TRUE) %>% 
  rowwise() %>% 
  mutate(inserted = paste0(str_split(pair, "")[[1]][1], inserted, str_split(pair, "")[[1]][2])) %>% 
  ungroup() %>% 
  deframe()

pairs_from_string <- function(text) {
  text_chars <- str_split(text, "")[[1]]
  head(paste0(text_chars, lead(text_chars)), -1)
}

string_from_pieces <- function(pieces) {
  map_chr(pieces, ~paste(str_split(., "")[[1]][1:2], collapse = "")) %>% 
    paste(collapse = "") %>% 
    paste0(., str_split(tail(pieces, 1), "")[[1]][3])
}

final <- reduce(1:10, function(prev, current) {
  string_from_pieces(rules[pairs_from_string(prev)])
}, .init = starting)

freqs <- table(str_split(final, "")[[1]])

max(freqs) - min(freqs)
