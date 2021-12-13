library(tidyverse)

input <- read_file("solutions_2021/input13.txt") %>% 
  str_split("\n\n") %>% pluck(1)

dots <- tibble(x = input[1]) %>% 
  separate_rows(x, sep = "\n") %>% 
  separate(x, c("x", "y"), convert = TRUE)

instructions <- tibble(x = input[2]) %>% 
  separate_rows(x, sep = "\n") %>% 
  filter(x != "") %>% 
  extract(x, c("axis", "value"), "fold along ([a-z])=(\\d+)", convert = TRUE)

map2(instructions$axis, instructions$value, function(axis, value) {
  if (axis == 'x') {
    dots <<- dots %>% 
      mutate(x = if_else(x <= value, x, 2L * value - x)) %>% 
      distinct(x, y)
  } else {
    dots <<- dots %>% 
      mutate(y = if_else(y <= value, y, 2L * value - y)) %>% 
      distinct(x, y)
  }
})

dots %>% 
  ggplot(aes(x = x, y = -y)) + 
  geom_tile()
