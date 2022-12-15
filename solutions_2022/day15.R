library(tidyverse)
library(glue)
library(memoise)
options(scipen = 999)
library(ivs)

year <- "2022"
day <- "15"
input_file <- glue("solutions_{year}/day{day}_input.txt")

target_y <- 2000000

input <- tibble(x = read_lines(input_file))

intervals <- input |>
  extract(
    x, c("xs", "ys", "xb", "yb"),
    "Sensor at x=([0-9-]+), y=([0-9-]+): closest beacon is at x=([0-9-]+), y=([0-9-]+)",
    convert = TRUE
  ) |>
  mutate(manh = abs(xs - xb) + abs(ys - yb)) |>
  mutate(dist_y = abs(ys - target_y)) |>
  mutate(
    left = if_else(manh - dist_y < 0, NA_real_, xs - (manh - dist_y)),
    right = if_else(manh - dist_y < 0, NA_real_, xs + (manh - dist_y) + 1)
  ) |>
  filter(!is.na(left)) |>
  mutate(target_iv = iv(left, right)) |>
  pull(target_iv)

tibble(x = iv_groups(intervals)) |>
  mutate(s = iv_start(x), e = iv_end(x)) |>
  mutate(l = e - s - 1) |>
  pull(l) |>
  sum()
