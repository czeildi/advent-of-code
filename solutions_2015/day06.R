library(tidyverse)

value_after_method_series <- function(methods) {
  reduce(methods, function(prev, current) {
    if (current == "turn on") return(prev + 1)
    if (current == "turn off") return(max(prev - 1, 0))
    return(prev + 2)
  }, .init = 0)
}

tibble(x = read_lines("solutions_2015/input06.txt")) %>% 
  extract(x, c("method", "x1", "y1", "x2", "y2"), "(.*) (\\d+),(\\d+) through (\\d+),(\\d+)", convert = TRUE) %>% 
  rowwise() %>% 
  mutate(rectangle_points = list(crossing(x = x1:x2, y = y1:y2))) %>% 
  unnest(rectangle_points) %>% 
  select(x, y, method) %>% 
  group_by(x, y) %>% 
  summarize(
    final_value = value_after_method_series(method)
  ) %>% 
  ungroup() %>% 
  summarize(num_turned_on = sum(final_value))
  
