library(tidyverse)
library(glue)
library(memoise)
options(scipen = 999)

year <- "2022"
day <- "09"
input_file <- glue("solutions_{year}/day{day}_input.txt")

steps <- read_lines(input_file) |> str_split(" ")

head_steps <- list(
  "R" = c(1, 0),
  "L" = c(-1, 0),
  "D" = c(0, 1),
  "U" = c(0, -1)
)

knot_steps <- list(
  "0;2" = c(0, 1),
  "0;-2" = c(0, -1),
  "2;0" = c(1, 0),
  "-2;0" = c(-1, 0),
  "1;2" = c(1, 1),
  "1;-2" = c(1, -1),
  "2;1" = c(1, 1),
  "-2;1" = c(-1, 1),
  "-1;2" = c(-1, 1),
  "-1;-2" = c(-1, -1),
  "2;-1" = c(1, -1),
  "-2;-1" = c(-1, -1),
  "2;2" = c(1, 1),
  "-2;-2" = c(-1, -1),
  "2;-2" = c(1, -1),
  "-2;2" = c(-1, 1)
)

knot_positions <- lapply(1:10, \(i) c(0, 0))
tail_visited <- list("0;0" = TRUE)

for (step in steps) {
  head_step <- head_steps[[step[1]]]
  for (i in seq_len(step[2])) {
    knot_positions[[1]] <- knot_positions[[1]] + head_step

    for (knot_idx in 2:10) {
      position_difference <- paste(
        knot_positions[[knot_idx - 1]] - knot_positions[[knot_idx]],
        collapse = ";"
      )
      if (position_difference %in% names(knot_steps)) {
        knot_positions[[knot_idx]] <- knot_positions[[knot_idx]] + knot_steps[[position_difference]]
      }
    }
    tail_visited[[paste(knot_positions[[10]], collapse = ";")]] <- TRUE
  }
}

length(tail_visited)
