library(tidyverse)
library(data.table)
options(scipen = 999)

year <- "2022"
day <- "03"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))

# part 1
ruck_items <- input |>
  mutate(n_item = nchar(x), ruck = row_number()) |>
  separate_rows(x, sep = "") |>
  filter(x != "")  |>
  group_by(ruck) |>
  mutate(rk = row_number()) |>
  ungroup() |>
  mutate(part = rk <= n_item / 2)

left_items <- filter(ruck_items, part == TRUE)
right_items <- filter(ruck_items, part == FALSE)

get_score <- function(letter) {
  ascii <- utf8ToInt(letter)
  if_else(ascii < 97, ascii - 38, ascii - 96)
}

inner_join(left_items, right_items, by  = c("x", "ruck")) |>
  distinct(ruck, x) |>
  mutate(value = map_dbl(x, get_score)) |>
  summarize(result = sum(value))



# part 2
group_racks <- input |>
  mutate(
    n_item = nchar(x),
    elf_group = rep(1:(nrow(input) / 3), each = 3),
    ruck = rep(1:3, nrow(input) / 3)
  ) |>
  separate_rows(x, sep = "") |>
  filter(x != "")

grouped_racks <- split(as.data.table(group_racks), by = "elf_group") |>
  lapply(as_tibble)

sapply(grouped_racks, function(racks) {
  i1 <- filter(racks, ruck == 1)
  i2 <- filter(racks, ruck == 2)
  i3 <- filter(racks, ruck == 3)

  inner_join(i1, i2, by = "x") |> 
    inner_join(i3, by = "x") |>
    pull(x) |>
    unique() |>
    get_score()
}) |> sum()
