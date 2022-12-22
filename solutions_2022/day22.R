library(tidyverse)
library(glue)
library(memoise)
library(zeallot)
options(scipen = 999)

year <- "2022"
day <- "22"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- read_file(input_file)
c(board, steps) %<-% str_split(input, "\n\n")[[1]]
steps <- str_replace(steps, "\n", "")

board_rows <- str_split(board, "\n")[[1]]
board_height <- length(board_rows)
board_width <- sapply(board_rows, nchar) |> max()

board_matrix <- lapply(str_split(board, "\n")[[1]], function(line) {
  prefix <- str_split(line, "")[[1]]
  row <- c(prefix, rep(" ", board_width - length(prefix)))
  row
}) |> do.call(rbind, args = _)

walks <- as.integer(str_extract_all(steps, "\\d+")[[1]])
turns <- str_extract_all(steps, "R|L")[[1]]
stopifnot(length(turns) == length(walks) - 1)

directions <- list(
  "n" = c(0, -1),
  "e" = c(1, 0),
  "s" = c(0, 1),
  "w" = c(-1, 0)
)

face_turns <- list(
  "n" = list("R" = "e", "L" = "w"),
  "e" = list("R" = "s", "L" = "n"),
  "s" = list("R" = "w", "L" = "e"),
  "w" = list("R" = "n", "L" = "s")
)

wrap <- function(x, y, dx, dy) {
  if (x + dx >= 1 && x + dx <= board_width) {
    if (y + dy == 0) return(c(x + dx, board_height))
    if (y + dy == (board_height + 1)) return(c(x + dx, 1))
    return(c(x + dx, y + dy))
  } else {
    if (x + dx == 0) return(c(board_width, y + dy))
    if (x + dx == (board_width + 1)) return(c(1, y + dy))
    return(c(x + dx, y + dy))
  }
}

find_next_pos <- function(x, y, dx, dy) {
  repeat {
    c(x, y) %<-% wrap(x, y, dx, dy)
    # cat("\n", y, x, board_matrix[y, x], "\n")
    if (board_matrix[y, x] != " ") break
  }
  c(x, y)
}

# initialize
y <- 1
face <- "e"
x <- 1
repeat {
  if (board_matrix[y, x] == " ") x <- x + 1
  else break
}
n_walk <- length(walks)
for (i in seq_len(n_walk)) {
  # walk
  c(dx, dy) %<-% directions[[face]]
  dist <- walks[i]
  for (s in seq_len(dist)) {
    c(next_pos_x, next_pos_y) %<-% find_next_pos(x, y, dx, dy)
    if (board_matrix[next_pos_y, next_pos_x] == "#") break
    x <- next_pos_x
    y <- next_pos_y
  }
  # turn
  if (i != n_walk) {
    face <- face_turns[[face]][[turns[i]]]
  }
}

1000 * y + 4 * x + match(face, c("e", "s", "w", "n")) - 1
