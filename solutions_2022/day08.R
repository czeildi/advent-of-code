library(tidyverse)
library(glue)
library(memoise)
options(scipen = 999)
source("utils.R")

year <- "2022"
day <- "08"
input_file <- glue("solutions_{year}/day{day}_input.txt")


input <- read_lines(input_file)
forest <- read_as_grid(input)

# part 1
forest |>
  group_by(y) |>
  arrange(x) |>
  mutate(l = value > lag(cummax(value))) |>
  arrange(desc(x)) |>
  mutate(r = value > lag(cummax(value))) |>
  group_by(x) |>
  arrange(y) |>
  mutate(t = value > lag(cummax(value))) |>
  arrange(desc(y)) |>
  mutate(b = value > lag(cummax(value))) |>
  filter(
    l | r | t | b | is.na(l) | is.na(r) | is.na(t) | is.na(b)
  ) |>
  nrow()
