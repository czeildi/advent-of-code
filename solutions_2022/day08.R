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

# part 2
acc <- function(value, max_height) {
  head(accumulate(value, .init = 0, function(out, curr) {
    if (curr >= max_height) return(1)
    return(out + 1)
  }), -1)
}

n_shorter_in_dir <- function(df) {
  df |>
  rowwise() |>
  mutate(
    of_max_height_before = list(lapply(0:9, \(i) acc(bef$value, i)))
  ) |>
  unnest_longer(col = of_max_height_before, indices_to = "height") |>
  mutate(height = height - 1) |>
  unnest_longer(col = all_of(c("bef", "of_max_height_before"))) |>
  unnest_wider(bef) |>
  filter(height == value)
}

to_left <- forest |>
  group_by(y) |>
  arrange(y, x) |>
  summarize(bef = list(data.frame(x, value))) |>
  n_shorter_in_dir() |>
  select(x, y, value, l = of_max_height_before)

to_right <- forest |>
  group_by(y) |>
  arrange(y, desc(x)) |>
  summarize(bef = list(data.frame(x, value))) |>
  n_shorter_in_dir() |>
  select(x, y, value, r = of_max_height_before)

to_top <- forest |>
  group_by(x) |>
  arrange(x, y) |>
  summarize(bef = list(data.frame(y, value))) |>
  n_shorter_in_dir() |>
  select(x, y, value, t = of_max_height_before)

to_bottom <- forest |>
  group_by(x) |>
  arrange(x, desc(y)) |>
  summarize(bef = list(data.frame(y, value))) |>
  n_shorter_in_dir() |>
  select(x, y, value, b = of_max_height_before)

scores <- to_left |>
  inner_join(to_right, by = c("x", "y", "value")) |>
  inner_join(to_top, by = c("x", "y", "value")) |>
  inner_join(to_bottom, by = c("x", "y", "value")) |>
  mutate(score = l * r * t * b) |>
  arrange(x, y)

max(scores$score)
