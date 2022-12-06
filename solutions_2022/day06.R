library(tidyverse)
library(glue)
library(memoise)
options(scipen = 999)

year <- "2022"
day <- "06"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))

input |>
  separate_rows(x, sep = "") |>
  filter(x != "") |>
  mutate(rn = row_number()) |>
  mutate(
    l1 = lag(x, 1), l2 = lag(x, 2), l3 = lag(x, 3), l4 = lag(x, 4), l5 = lag(x, 5),
    l6 = lag(x, 6), l7 = lag(x, 7), l8 = lag(x, 8), l9 = lag(x, 9),
    l10 = lag(x, 10), l11 = lag(x, 11), l12 = lag(x, 12), l13 = lag(x, 13)
  ) |>
  rowwise() |>
  mutate(cd = length(unique(c(x, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13)))) |>
  filter(cd == 14)
