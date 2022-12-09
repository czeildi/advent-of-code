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

tail_steps <- list(
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
  "-2;-1" = c(-1, -1)
)

h_chain <- c(0, 0)
t_chain <- c(0, 0)
visited <- list("0;0" = TRUE)

for (step in steps) {
  head_step <- head_steps[[step[1]]]
  for (i in seq_len(step[2])) {
    h_chain <- h_chain + head_step
    position_difference <- paste(h_chain - t_chain, collapse = ";")
    if (position_difference %in% names(tail_steps)) {
      t_chain <- t_chain + tail_steps[[position_difference]]
      visited[[paste(t_chain, collapse = ";")]] <- TRUE
    }
  }
}

length(visited)
