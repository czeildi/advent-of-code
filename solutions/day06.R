library(tidyverse)

answers <- tibble(x = read_file("solutions/day06_input.txt")) %>% 
  separate_rows(x, sep = "\n\n")


# part 1 ------------------------------------------------------------------

answers %>% 
  mutate(
  letters = map_int(x, ~{
    str_replace_all(., "\n", "") %>% 
      str_split('') %>% 
      .[[1]] %>% 
      unique() %>% 
      length()
  })
) %>% summarize(s = sum(letters))


# part 2 ------------------------------------------------------------------

by_person <- answers %>% 
  mutate(group_id = seq_len(nrow(.))) %>% 
  separate_rows(x, sep = "\n") %>% 
  mutate(person_id = seq_len(nrow(.))) %>% 
  separate_rows(x, sep = "") %>% 
  filter(x != "") 

by_person %>% 
  group_by(group_id) %>% 
  mutate(group_size = n_distinct(person_id)) %>% 
  group_by(x, group_id) %>% 
  mutate(letter_count = n_distinct(person_id)) %>% 
  filter(letter_count == group_size) %>% 
  select(x, group_id) %>% 
  distinct() %>% 
  nrow(.)
  
