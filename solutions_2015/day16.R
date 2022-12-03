library(tidyverse)
library(glue)

year <- "2015"
day <- "16"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))

target <-
"children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1"

aunt <- tibble(x = str_split(target, "\n")[[1]]) |>
  extract(x, c("prop", "val"), regex = "([a-z]+): (\\d+)", convert = TRUE)

input |>
  extract(x, c("id", "props"), regex = "Sue (\\d+): (.*)", convert = TRUE) |>
  separate_rows(props, sep = ", ") |>
  extract(props, c("prop", "val"), regex = "([a-z]+): (\\d+)", convert = TRUE) |>
  left_join(aunt, by = "prop") |>
  group_by(id) |>
  filter(n() == sum(val.x == val.y))

# the cats and trees readings indicates that there are greater than that many
# pomeranians and goldfish readings indicate that there are fewer than that many

target <-
"children: 3,eq
cats: 7,gt
samoyeds: 2,eq
pomeranians: 3,lt
akitas: 0,eq
vizslas: 0,eq
goldfish: 5,lt
trees: 3,gt
cars: 2,eq
perfumes: 1,eq"

aunt <- tibble(x = str_split(target, "\n")[[1]]) |>
  extract(x, c("prop", "val", "op"), regex = "([a-z]+): (\\d+),(\\w+)", convert = TRUE)

input |>
  extract(x, c("id", "props"), regex = "Sue (\\d+): (.*)", convert = TRUE) |>
  separate_rows(props, sep = ", ") |>
  extract(props, c("prop", "val"), regex = "([a-z]+): (\\d+)", convert = TRUE) |>
  left_join(aunt, by = "prop") |>
  mutate(is_match = case_when(
    op == "eq" ~ val.x == val.y,
    op == "gt" ~ val.x > val.y,
    op == "lt" ~ val.x < val.y
  )) |>
  group_by(id) |>
  filter(n() == sum(is_match == TRUE))
