library(tidyverse)

# part 1

read_lines("solutions_2017/day04_input.txt") %>% 
  keep(function(passphrase) {
    words <- str_split(passphrase, " ")[[1]]
    n_distinct(words) == length(words)
  }) %>% 
  length()

# part 2

sort_word <- function(word) {
  paste(sort(str_split(word, "")[[1]]), collapse = "")
}

read_lines("solutions_2017/day04_input.txt") %>% 
  keep(function(passphrase) {
    words <- str_split(passphrase, " ")[[1]]
    sorted_words <- map_chr(words, sort_word)
    n_distinct(sorted_words) == length(sorted_words)
  }) %>% 
  length()
