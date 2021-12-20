library(tidyverse)

calculate_checksum <- function(room) {
  str_replace_all(room, "-", "") %>% 
    str_split("") %>% 
    pluck(1) %>% 
    table() %>% 
    enframe() %>% 
    arrange(desc(value), name) %>% 
    head(5) %>% 
    pull(name) %>% 
    str_c(collapse = "")
}

# part 1

tibble(x = read_lines("solutions_2016/input04.txt")) %>% 
  extract(x, into = c("room", "id", 'checksum'), regex = "([a-z-]+)(\\d+)\\[([a-z]+)\\]", convert = TRUE) %>% 
  rowwise() %>% 
  filter(
    calculate_checksum(room) == checksum
  ) %>% 
  pull(id) %>% 
  sum()

# part 2

shift_letter <- function(letter, id) {
  if (letter == " ") return(" ")
  idx = which(letters == letter)
  new_idx = if_else((idx + id) %% 26 == 0, 26, (idx + id) %% 26)
  letters[new_idx]
}

decrypt <- function(room, id) {
  str_replace_all(room, "-", " ") %>% 
    str_split("") %>% 
    pluck(1) %>% 
    map_chr(~shift_letter(., id)) %>% 
    str_c(collapse = "")
}

room_names <- tibble(x = read_lines("solutions_2016/input04.txt")) %>% 
  extract(x, into = c("room", "id", 'checksum'), regex = "([a-z-]+)(\\d+)\\[([a-z]+)\\]", convert = TRUE) %>% 
  rowwise() %>% 
  filter(
    calculate_checksum(room) == checksum
  ) %>% 
  mutate(name = decrypt(room, id))

room_names %>% 
  filter(str_detect(name, "north"))