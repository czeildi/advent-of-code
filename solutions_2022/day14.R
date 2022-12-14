library(tidyverse)
library(glue)
library(memoise)
library(ggplot2)
options(scipen = 999)
theme_set(theme_bw())

year <- "2022"
day <- "14"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))

cave <- input |>
  mutate(rock_id = row_number()) |>
  separate_rows(x, sep = " -> ") |>
  group_by(rock_id) |>
  mutate(line_id = row_number()) |>
  separate(x, c("x", "y"), convert = TRUE) |>
  mutate(xp = lag(x), yp = lag(y)) |>
  rowwise() |>
  mutate(xp = replace_na(xp, x), yp = replace_na(yp, y)) |>
  summarise(x = list(x:xp), y = list(y:yp), .groups = "drop") |>
  unnest_longer(col = c(x, y)) |>
  distinct()

ggplot(cave, aes(x, -y)) +
  geom_tile() +
  coord_fixed() +
  geom_vline(xintercept = 500, linetype = "dashed")
