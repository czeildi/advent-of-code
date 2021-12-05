library(tidyverse)

input <- read_lines("solutions_2015/input01.txt")

input %>% 
  str_split("") %>% .[[1]] %>% 
  tibble(x = .) %>% 
  count(x) %>% 
  pivot_wider(names_from = x, values_from = n) %>% 
  mutate(res = `(` - `)`)

# part 2

directions <- tribble(
  ~direction, ~h,
  "(", 1,
  ")", -1
)

tibble(direction = input) %>% 
  separate_rows(direction, sep = "") %>% 
  filter(direction != "") %>% 
  mutate(idx = 1:n()) %>% 
  inner_join(directions) %>% 
  mutate(floor = cumsum(h)) %>% 
  filter(floor == -1) %>% 
  head(1) %>% 
  pull(idx)
