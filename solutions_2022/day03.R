library(tidyverse)
library(glue)
library(memoise)
library(data.table)
options(scipen = 999)

year <- "2022"
day <- "03"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))

# part 1
ruck_items <- input |>
  mutate(n_item = nchar(x), ruck = 1:n()) |>
  separate_rows(x, sep = "") |>
  filter(x != "")  |>
  group_by(ruck) |>
  mutate(rk = 1:n()) |>
  ungroup() |>
  mutate(part = rk <= n_item / 2)

left_items <- filter(ruck_items, part == TRUE)
right_items <- filter(ruck_items, part == FALSE)

inner_join(left_items, right_items, by  = c("x", "ruck")) |>
  select(x, ruck) |>
  distinct() |>
  rowwise() |>
  mutate(value = if_else(
    utf8ToInt(x) < 97, utf8ToInt(x) - 38, utf8ToInt(x) - 96
  )) |>
  pull(value) |>
  sum()



# part 2
group_racks <- input |>
  mutate(
    n_item = nchar(x),
    elf_group = rep(1:(nrow(input) / 3), each = 3),
    ruck = rep(1:3, nrow(input) / 3)
  ) |>
  separate_rows(x, sep = "") |>
  filter(x != "")

grouped_racks <- lapply(split(as.data.table(group_racks), by = "elf_group"), as_tibble)

sapply(grouped_racks, function(racks) {
  i1 <- filter(racks, ruck == 1)
  i2 <- filter(racks, ruck == 2)
  i3 <- filter(racks, ruck == 3)

  common_value <- i1 |>
    inner_join(i2, by = "x") |> inner_join(i3, by = "x") |>
    select(x) |>
    distinct() |>
    pull(x)

  score <- if_else(
    utf8ToInt(common_value) < 97,
    utf8ToInt(common_value) - 38,
    utf8ToInt(common_value) - 96
  )
}) |> sum()
