library(tidyverse)
library(memoise)
options(scipen = 999)

input_file <- "solutions_2022/day02_input.txt"
input <- tibble(x = read_lines(input_file))

get_score <- function(t) {
  t |>
    mutate(
      win_score = case_when(
        P1 == P2 ~ 3,
        (P1 - P2) %% 3 == 1 ~ 0,
        (P1 - P2) %% 3 == 2 ~ 6
      )
    ) |>
    mutate(score = win_score + P2) |>
    pull(score) |>
    sum(na.rm = TRUE)
}

# part 1
input |>
  separate(x, into = c("P1", "P2")) |>
  rowwise() |>
  mutate(
    P1 = utf8ToInt(P1) - 64, # A -> 1
    P2 = utf8ToInt(P2) - 87 # X -> 1
  ) |>
  get_score()

# part 2

# from utils
one_indexed_remainder <- function(value, mod) {
  if_else(value %% mod == 0, mod, value %% mod)
}

input |>
  separate(x, into = c("P1", "res")) |>
  rowwise() |>
  mutate(
    P1 = utf8ToInt(P1) - 64
  ) |>
  mutate(
    P2 = case_when(
      res == "Y" ~ P1,
      res == "X" ~ one_indexed_remainder(P1 - 1, 3),
      res == "Z" ~ one_indexed_remainder(P1 + 1, 3)
    )
  ) |>
  get_score()
