library(tidyverse)

rectangles <- tibble(x = read_lines('./solutions_2018/day03_input.txt')) %>% 
  extract(x, c("left", "top", "width", "height"), regex = "#\\d+ @ (\\d+),(\\d+): (\\d+)x(\\d+)", convert = TRUE) %>% 
  mutate(right = left + width - 1, bottom = top + height - 1) %>% 
  select(-c(width, height))

MIN_X = min(rectangles$left)
MAX_X = max(rectangles$right)

num_multiple_claims_in_columns <- function(rectangles, current_x) {
  relevant_rectangles <- rectangles %>% 
    filter(left <= current_x & right >= current_x) 
  
  change_points <- relevant_rectangles %>% 
    select(top, bottom) %>% 
    pivot_longer(c(top, bottom)) %>% 
    group_by(name, value) %>% 
    summarize(num_rect = n(), .groups = "drop") %>% 
    arrange(value, desc(name)) %>% 
    mutate(distance = lead(value) - value) %>% 
    mutate(sign = if_else(name == 'top', 1, -1)) %>% 
    mutate(num_claims = cumsum(sign * num_rect)) %>% 
    mutate(prev_num_claims = lag(num_claims))
  
  change_points %>% 
    mutate(contribution = if_else(
      num_claims >= 2, 
      distance, 
      if_else(num_claims < 2 & prev_num_claims >= 2, 1, 0)
    )) %>% 
    pull(contribution) %>% 
    sum(na.rm = TRUE)  
}

sum(map_dbl(MIN_X:MAX_X, ~ num_multiple_claims_in_columns(rectangles, .x)))

