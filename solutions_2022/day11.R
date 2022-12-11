library(tidyverse)
library(glue)
options(scipen = 999)

year <- "2022"
day <- "11"
input_file <- glue("solutions_{year}/day{day}_input_sample.txt")

input <- read_file(input_file)
monkeys <- str_split(input, "\n\n")[[1]]

parse_monkey <- function(monkey) {
  lines <- str_split(monkey, "\n")[[1]]
  list(
    id = as.integer(str_extract(lines[1], "\\d+")) + 1,
    items = as.integer(str_split(str_replace(lines[2], "  Starting items: ", ""), ", ")[[1]]),
    op = str_replace(lines[3], "  Operation: new = ", ""),
    test = as.integer(str_extract(lines[4], "\\d+")),
    pass = as.integer(str_extract(lines[5], "\\d+")) + 1,
    fail = as.integer(str_extract(lines[6], "\\d+")) + 1,
    inspected = 0
  )
}

monkey_states <- lapply(monkeys, parse_monkey)

mod <- Reduce(`*`, sapply(monkey_states, function(ms) ms$test))

n_rounds <- c(20, 10000)
worry_managers <- list(
  \(x) x %/% 3,
  \(x) x %% mod
)

part_idx <- 2

for (round in seq_len(n_rounds[part_idx])) {
  for (monkey_id in seq_along(monkey_states)) {
    for (old in monkey_states[[monkey_id]]$items) {
      monkey_states[[monkey_id]]$inspected <- monkey_states[[monkey_id]]$inspected + 1

      worry <- eval(parse(text = monkey_states[[monkey_id]]$op))
      managed_worry <- worry_managers[[part_idx]](worry)

      passed_condition <- managed_worry %% monkey_states[[monkey_id]]$test == 0
      next_monkey <- if_else(
        passed_condition,
        monkey_states[[monkey_id]]$pass,
        monkey_states[[monkey_id]]$fail
      )
      monkey_states[[next_monkey]]$items <- c(monkey_states[[next_monkey]]$items, managed_worry)
    }
    monkey_states[[monkey_id]]$items <- integer(0)
  }
}

sapply(monkey_states, \(ms) ms$inspected) |>
  sort(decreasing = TRUE) |> head(2) |> Reduce(`*`, x = _)
