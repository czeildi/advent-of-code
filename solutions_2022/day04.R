library(tidyverse)
library(glue)

year <- "2022"
day <- "04"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))

input |>
  separate(x, c("x1", "x2", "y1", "y2"), convert = TRUE) |>
  mutate(overlap = (x1 <= y2 & x1 >= y1) | (x2 >= y1 & x2 <= y2) | (y1 >= x1 & y1 <= x2)) |>
  summarize(sum(overlap))
