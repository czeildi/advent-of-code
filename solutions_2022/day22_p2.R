library(tidyverse)
library(glue)
library(zeallot)
options(scipen = 999)

year <- "2022"
day <- "22"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- read_file(input_file)
c(board, steps) %<-% str_split(input, "\n\n")[[1]]
steps <- str_replace(steps, "\n", "")


board_rows <- str_split(board, "\n")[[1]]
board_matrix <- lapply(board_rows, function(line) {
  prefix <- str_split(line, "")[[1]]
  row <- c(prefix, rep(" ", 3 * 50 - length(prefix)))
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

# this hardcodes move to a different face of the cube for my inpuy
wrap <- function(x, y, dx, dy, face, verbose = FALSE) {
  if (y + dy == 0 && x + dx <= 100) {
    if (verbose) cat("# 1. 1M ^ 4L north -> east", "\n")
    return(c(1, x + dx + 100, "e"))
  }
  if (y + dy == 0 && x + dx > 100) {
    if (verbose) cat("# 2. 1R ^ 4L north -> north", "\n")
    return(c(x + dx - 100, 200, "n"))
  }
  if (x + dx == 50 && y + dy <= 50) {
    if (verbose) cat("# 3. 1M < 3L west -> east", "\n")
    return(c(1, 151 - y - dy, "e"))
  }
  if (x + dx == 151 && y + dy <= 50) {
    if (verbose) cat("# 4. 1R > 3M east -> west", "\n")
    return(c(100, 151 - y - dy, "w"))
  }
  if (y + dy == 51 && x + dx >= 101) {
    if (verbose) cat("# 5. 1R . 2M south -> west", "\n")
    return(c(100, x + dx - 50,  "w"))
  }
  if (x + dx == 50 && y + dy <= 100) {
    if (verbose) cat("# 6. 2M < 3L west -> south", "\n")
    return(c(y + dy - 50, 101, "s"))
  }
  if (x + dx == 101 && y + dy <= 100) {
    if (verbose) cat("# 7. 2M > 1R east -> north", "\n")
    return(c(y + dy + 50, 50, "n"))
  }
  if (y + dy == 100 && x + dx <= 50) {
    if (verbose) cat("# 8. 3L ^ 2M north -> east", "\n")
    return(c(51, x + dx + 50, "e"))
  }
  if (x + dx == 0 && y + dy <= 150) {
    if (verbose) cat("# 9. 3L < 1M west -> east", "\n")
    return(c(51, 151 - y - dy, "e"))
  }
  if (x + dx == 101 && y + dy <= 150) {
    if (verbose) cat("# 10. 3M > 1R east -> west", "\n")
    return(c(150, 151 - y - dy, "w"))
  }
  if (y + dy == 151 && x + dx >= 51) {
    if (verbose) cat("# 11. 3M . 4L south -> west", "\n")
    return(c(50, x + dx + 100, "w"))
  }
  if (y + dy >= 151 && x + dx == 0) {
    if (verbose) cat("# 12. 4L < 1M west -> south", "\n")
    return(c(y + dy - 100, 1, "s"))
  }
  if (y + dy >= 151 && x + dx == 51) {
    if (verbose) cat("# 13. 4L > 3M east -> north", "\n")
    return(c(y + dy - 100, 150, "n"))
  }
  if (y + dy == 201 && x + dx <= 50) {
    if (verbose) cat("# 14. 4L . 1R south -> south", "\n")
    return(c(x + dx + 100, 1, "s"))
  }
  return(c(x + dx, y + dy, face))
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

# do the moves
for (i in seq_len(n_walk)) {
  # walk
  dist <- walks[i]
  for (s in seq_len(dist)) {
    c(dx, dy) %<-% directions[[face]]
    c(next_pos_x, next_pos_y, new_face) %<-% wrap(x, y, dx, dy, face)
    next_pos_x <- as.integer(next_pos_x)
    next_pos_y <- as.integer(next_pos_y)

    if (board_matrix[next_pos_y, next_pos_x] == ".") {
      x <- next_pos_x
      y <- next_pos_y
      face <- new_face
    }
  }
  # turn
  if (i != n_walk) {
    face <- face_turns[[face]][[turns[i]]]
  }
}

1000 * y + 4 * x + match(face, c("e", "s", "w", "n")) - 1
