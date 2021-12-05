library(tidyverse)

input <- read_lines("solutions_2021/day04_input.txt")

bingo_numbers <- input[1] %>% str_split(",") %>% .[[1]] %>% as.numeric()

bingo_sheets <- tail(input, -2) %>% 
  .[. != ""] %>% 
  tibble(x = .) %>% 
  mutate(sheet_id = rep(1:(nrow(.)/5), each = 5)) %>% 
  mutate(row_id = rep(1:5, nrow(.)/5)) %>% 
  separate_rows(x, convert = TRUE) %>% 
  filter(!is.na(x)) %>% 
  mutate(col_id = rep(1:5, nrow(.)/5))

sheet_wins <- bingo_sheets %>% 
  rowwise() %>% 
  mutate(idx = which(bingo_numbers == x)) %>% 
  group_by(sheet_id, row_id) %>% 
  mutate(row_wins_at = max(idx)) %>% 
  group_by(sheet_id, col_id) %>% 
  mutate(col_wins_at = max(idx)) %>% 
  group_by(sheet_id) %>% 
  mutate(sheet_wins_at = min(min(row_wins_at, na.rm = TRUE), min(col_wins_at, na.rm = TRUE), na.rm = TRUE)) %>% 
  ungroup() 
  
# part 1

winner <- sheet_wins %>% 
  mutate(win_idx = min(sheet_wins_at, na.rm = TRUE)) %>% 
  filter(sheet_wins_at == win_idx) 

unmarked_sum <- winner %>% 
  filter(idx > win_idx) %>% 
  pull(x) %>% 
  sum()

last_number <- winner %>% filter(idx == win_idx) %>% pull(x)

unmarked_sum * last_number  

# part 2

last_winner <- sheet_wins %>% 
  mutate(win_idx = max(sheet_wins_at)) %>% # only difference from part 1 is max instead of min
  filter(sheet_wins_at == win_idx)

unmarked_sum <- last_winner %>% 
  filter(idx > win_idx) %>% 
  pull(x) %>% 
  sum()

last_number <- last_winner %>% filter(idx == win_idx) %>% pull(x)

unmarked_sum * last_number  