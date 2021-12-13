library(tidyverse)

input <- str_split(read_file("solutions_2021/input13.txt"), "\n\n")[[1]]

dots <- tibble(x = input[1]) %>% 
  separate_rows(x, sep = "\n") %>% 
  separate(x, c("x", "y"), convert = TRUE)

instructions <- tibble(x = input[2]) %>% 
  separate_rows(x, sep = "\n") %>% 
  filter(x != "") %>% 
  extract(x, c("axis", "value"), "fold along ([a-z])=(\\d+)", convert = TRUE)

walk2(instructions$axis, instructions$value, function(axis, value) {
  dots <<- dots %>% 
    mutate(
      x = if_else(x <= value | axis == 'y', x, 2L * value - x),
      y = if_else(y <= value | axis == 'x', y, 2L * value - y)
    ) %>% 
    distinct(x, y)
})

ggplot(dots, aes(x = x, y = -y)) + 
  geom_tile()
