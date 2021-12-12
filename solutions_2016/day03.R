library(tidyverse)

input <- tibble(x = read_lines("solutions_2016/input03.txt")) %>% 
  separate(x, c('dummy', 'a', 'b', 'c'), convert = TRUE) %>% 
  select(-dummy) 

input %>% 
  filter(a + b > c & b + c > a & c + a > b) %>% 
  nrow()

input %>% 
  {tibble(x = c(.$a, .$b, .$c))} %>% 
  mutate(
    triangle_id = rep(1:(nrow(.)/3), each = 3),
    side_id = rep(c('a', 'b', 'c'), nrow(.)/3)
  ) %>% 
  pivot_wider(id_cols = c(triangle_id), names_from = side_id, values_from = x) %>% 
  filter(a + b > c & b + c > a & c + a > b) %>% 
  nrow()
