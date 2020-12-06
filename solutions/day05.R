library(tidyverse)

boarding_passes <- tibble(bp = read_lines("solutions/day05_input.txt")) %>% 
  mutate(bp = str_replace_all(bp, "F", "0")) %>% 
  mutate(bp = str_replace_all(bp, "B", "1")) %>% 
  mutate(bp = str_replace_all(bp, "L", "0")) %>% 
  mutate(bp = str_replace_all(bp, "R", "1"))

# part 1 ------------------------------------------------------------------

seat_ids <- boarding_passes %>% 
  mutate(seat_id = map_dbl(bp, ~{
    digits <- rev(as.integer(str_split(.x, '')[[1]]))
    values <- 2 ^ (seq_along(digits) - 1)
    sum(digits * values)
  })) %>% 
  arrange(desc(seat_id))

# part 2 ------------------------------------------------------------------

set_ids %>% 
  mutate(lead = lead(seat_id)) %>% 
  filter(seat_id - lead > 1)
