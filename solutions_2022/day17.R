library(tidyverse)
library(glue)
options(scipen = 999)
library(ggplot2)

year <- "2022"
day <- "17"
input_file <- glue("solutions_{year}/day{day}_input.txt")

wind <- if_else(head(str_split(read_file(input_file), "")[[1]], -1) == "<", -1, 1)
wind_size <- length(wind)
# heuristical number
n_tetris <- 40000
# heuristic: we do not need to keep track of more than 100 rows
cave <- matrix(logical(7 * 100), ncol = 7)
cave[1, ] <- TRUE

tetrises <- list(
#' ####
  cbind(
    c(3, 3, 3, 3),
    c(3, 4, 5, 6)
  ),
#' .#.
#' ###
#' .#.
  cbind(
    c(4, 3, 4, 5, 4),
    c(3, 4, 4, 4, 5)
  ),
#' ..#
#' ..#
#' ###
  cbind(
    c(3, 3, 3, 4, 5),
    c(3, 4, 5, 5, 5)
  ),
#' #
#' #
#' #
#' #
  cbind(
    c(3, 4, 5, 6),
    c(3, 3, 3, 3)
  ),
#' ##
#' ##
  cbind(
    c(3, 4, 3, 4),
    c(3, 3, 4, 4)
  )
)

print_cave <- function(cave) {
  rows <- apply(
    cave, 1,
    \(x) paste(if_else(if_else(x == 1, "@", "-")), collapse = "")
  )
  cat(paste(rev(rows), collapse = "\n"), "\n\n")
}

highest_rock_position <- function(cave) {
  max(which(apply(cave, 1, max) > 0))
}

move_with_wind <- function(cave, tetris, wind_direction) {
  would_be_tetris <- cbind(tetris[, 1], tetris[, 2] + wind_direction)
  if (min(would_be_tetris[, 2]) < 1 || max(would_be_tetris[, 2]) > 7) {
    return(tetris)
  }
  blocked_by_rock <- FALSE
  for (i in seq_len(nrow(would_be_tetris))) {
    if (cave[would_be_tetris[i, 1], would_be_tetris[i, 2]] != 0) {
      blocked_by_rock <- TRUE
    }
  }
  if (blocked_by_rock) return(tetris)
  tetris[, 2] <- tetris[, 2] + wind_direction
  tetris
}

is_blocked_from_below <- function(tetris) {
  is_blocked <- FALSE
  for (i in seq_len(nrow(tetris))) {
    if (cave[tetris[i, 1] - 1, tetris[i, 2]] != 0) {
      is_blocked <- TRUE
    }
  }
  is_blocked
}

move_down <- function(tetris) {
  tetris[, 1] <- tetris[, 1] - 1
  tetris
}

get_new_tetris <- function(cave, tetris_idx) {
  new_tetris <- tetrises[[tetris_idx]]
  new_tetris[, 1] <- new_tetris[, 1] + highest_rock_position(cave) + 1
  new_tetris
}

fill_cave_with_tetris <- function(cave, tetris) {
  for (i in seq_len(nrow(tetris))) {
    cave[tetris[i, 1], tetris[i, 2]] <- TRUE
  }
  cave
}

fallen_tetris_count <- 0
tetris_idx <- 1
wind_idx <- 1
current_tetris <- get_new_tetris(cave, tetris_idx)
max_heights <- integer(n_tetris)
result <- -1 # floor does not count

system.time({
repeat {
  current_tetris <- move_with_wind(cave, current_tetris, wind[wind_idx])
  wind_idx <- wind_idx + 1
  if (wind_idx == wind_size + 1) wind_idx <- 1

  is_fallen <- is_blocked_from_below(current_tetris)
  if (is_fallen) {
    cave <- fill_cave_with_tetris(cave, current_tetris)
    fallen_tetris_count <- fallen_tetris_count + 1
    # heuristic that only the top few rows count
    highest <- highest_rock_position(cave)
    max_heights[fallen_tetris_count] <- result + highest
    if (highest >= 75) {
      result <- result + 25
      cave <- cave[-1:-25, ]
      cave[1, ] <- TRUE
      cave <- rbind(cave, matrix(logical(7 * 25), ncol = 7))
    }
    if (fallen_tetris_count == n_tetris) break
    tetris_idx <- tetris_idx + 1
    if (tetris_idx == 6) tetris_idx <- 1
    current_tetris <- get_new_tetris(cave, tetris_idx)
    # print_cave(fill_cave_with_tetris(cave, current_tetris))
  } else {
    current_tetris <- move_down(current_tetris)
  }
}
})

# part 1: run with n_tetris = 2022
tail(max_heights, 1)

# part 2

# empirical search for a repeating period in the growth pattern
height_diffs <- max_heights - lag(max_heights)
initial_slice <- 999
period_search_space <- height_diffs[(initial_slice + 1):n_tetris]
period_length <- NULL
# another heuristic, hope the period is shorter...
for (i in 1:1000) {
  zero_positions <- which(period_search_space == 0)
  # if besides first NA value, only 1 unique value, we hopefully found a period
  if (length(unique(zero_positions - lag(zero_positions, i))) == 2) {
    period_length <- unique(zero_positions - lag(zero_positions, i))[2]
    break
  }
}

print(period_length)
n_cycle <- (10^12 - initial_slice) %/% period_length
remainder <- (10^12 - initial_slice) %% period_length

sum(period_search_space[1:period_length]) * n_cycle +
  sum(period_search_space[1:remainder]) +
  max_heights[initial_slice]
