library(tidyverse)
library(glue)
options(scipen = 999)
library(ggplot2)

year <- "2022"
day <- "17"
input_file <- glue("solutions_{year}/day{day}_input_sample.txt")

wind <- if_else(head(str_split(read_file(input_file), "")[[1]], -1) == "<", -1, 1)
wind_size <- length(wind)
n_tetris <- 2022
estimated_cave_height <- (n_tetris %/% 5 + 1) * 13 + 100
cave <- matrix(integer(7 * estimated_cave_height), ncol = 7)
cave[1, ] <- 2

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
    \(x) paste(if_else(x == 2, "W", if_else(x == 1, "@", "-")), collapse = "")
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
    cave[tetris[i, 1], tetris[i, 2]] <- 1
  }
  cave
}

fallen_tetris_count <- 0
tetris_idx <- 1
wind_idx <- 1
current_tetris <- get_new_tetris(cave, tetris_idx)
# print_cave(fill_cave_with_tetris(cave, current_tetris))

repeat {
  current_tetris <- move_with_wind(cave, current_tetris, wind[wind_idx])
  # print_cave(fill_cave_with_tetris(cave, current_tetris))
  wind_idx <- wind_idx + 1
  if (wind_idx == wind_size + 1) wind_idx <- 1

  is_fallen <- is_blocked_from_below(current_tetris)
  if (is_fallen) {
    cave <- fill_cave_with_tetris(cave, current_tetris)
    # print_cave(cave)
    fallen_tetris_count <- fallen_tetris_count + 1
    if (fallen_tetris_count == n_tetris) break
    tetris_idx <- tetris_idx + 1
    if (tetris_idx == 6) tetris_idx <- 1
    current_tetris <- get_new_tetris(cave, tetris_idx)
    # print_cave(fill_cave_with_tetris(cave, current_tetris))
  } else {
    current_tetris <- move_down(current_tetris)
    # print_cave(fill_cave_with_tetris(cave, current_tetris))
  }
}

highest_rock_position(cave) - 1
