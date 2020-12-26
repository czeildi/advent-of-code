library(tidyverse)

input <- readr::read_lines("solutions_2020/day02_input.txt")
passwords <- tibble(input = input) %>% 
  separate(input, c("min", "max", "letter", "password"), convert = TRUE) 


# part 1 ------------------------------------------------------------------

passwords %>% 
  mutate(is_correct = pmap_lgl(., function(min, max, letter, password) {
    min <= str_count(password, letter) && max >= str_count(password, letter)
  })) %>% 
  count(is_correct)


# part 2 ------------------------------------------------------------------

passwords %>% 
  mutate(is_correct = pmap_lgl(., function(min, max, letter, password) {
    password_chars <- str_split(password, '')[[1]]
    ((password_chars[min] == letter) + (password_chars[max] == letter)) == 1
  })) %>% 
  count(is_correct)
