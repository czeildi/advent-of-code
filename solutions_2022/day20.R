library(tidyverse)
library(glue)
options(scipen = 999)

year <- "2022"
day <- "20"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- scan(input_file, sep = "\n")
# next line only for part 2
input <- input * 811589153
n <- length(input)

new_pos <- function(old_pos, move, n) {
  move_mod <- move %% (n - 1)
  if (move_mod == 0) return(old_pos)
  to_candidate <- (old_pos + move_mod)
  if (to_candidate >= n && move > 0) return(to_candidate - n + 1)
  if (to_candidate > n && move < 0) return(to_candidate - n + 1)
  return(to_candidate)
}

# lightweight unit tests
stopifnot(new_pos(1, 4, 7) == 5)
stopifnot(new_pos(3, 3, 7) == 6)
stopifnot(new_pos(3, 4, 7) == 1)
stopifnot(new_pos(3, 6, 7) == 3)
stopifnot(new_pos(3, 12, 7) == 3)
stopifnot(new_pos(3, 0, 7) == 3)
stopifnot(new_pos(3, -1, 7) == 2)
stopifnot(new_pos(3, -2, 7) == 7)
stopifnot(new_pos(3, -11, 7) == 4)

# for debugging of sample case
print_order <- function() {
  tibble(input, pos = .globals$current_positions) |>
    arrange(pos) |> pull(input) |>
    cat("\n")
}

.globals <- new.env()
parent.env(.globals) <- emptyenv()
.globals$current_positions <- seq_len(n)

move_element <- function(idx_to_move) {
  value_to_move <- input[idx_to_move]
  move_from <- .globals$current_positions[idx_to_move]

  move_to <- new_pos(move_from, value_to_move, n)

  if (move_from < move_to) {
    .globals$current_positions <- if_else(
      .globals$current_positions > move_from & .globals$current_positions <= move_to,
      .globals$current_positions - 1L,
      .globals$current_positions
    )
    .globals$current_positions[idx_to_move] <- move_to
  }
  if (move_from > move_to) {
    .globals$current_positions <- if_else(
      .globals$current_positions < move_from & .globals$current_positions >= move_to,
      .globals$current_positions + 1L,
      .globals$current_positions
    )
    .globals$current_positions[idx_to_move] <- move_to
  }
  # only debug for small sample input
  if (n == 7) {
    print_order()
  }
}

# 10x loop only for part 2
for (x in 1:10) {
  for (i in seq_len(n)) {
    move_element(i)
  }
}

final <- tibble(input, pos = .globals$current_positions) |>
    arrange(pos) |>
    pull(input)

zero_pos <- which(final == 0)
positions <- (zero_pos + c(1000, 2000, 3000)) %% n
positions[positions == 0] <- n
sum(final[positions])
