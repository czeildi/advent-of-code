library(tidyverse)
library(glue)
library(zeallot)
options(scipen = 999)

year <- "2022"
day <- "05"
input_file <- glue("solutions_{year}/day{day}_input.txt")

# manual parsing
n_col <- 9 # 3 for sample data

input <- read_file(input_file)
c(crates, commands) %<-% str_split(input, "\n\n")[[1]]

comms <- tibble(x = str_split(commands, "\n")[[1]]) |>
  filter(x != "") |>
  extract(x, c("n", "from", "to"), "move (\\d+) from (\\d+) to (\\d+)", convert = TRUE)

crate_matrix <- str_split(crates, "\n")[[1]] |>
  head(-1) |>
  str_split_fixed("   |\\[", n_col + 1)

# dirty manual hack of fixing not being able to parse input correctly...
if (n_col == 9) {
  crate_matrix[1, 7] <- "L"
  crate_matrix[1, 8] <- ""
}

crate_positions <- lapply(1:n_col, function(i) {
  x <- gsub(" |\\]|\\[", "", crate_matrix[, i + 1])
  x[x != ""]
})

do_1_move <- function(crate_positions, n, from, to) {
  to_move <- crate_positions[[from]][1:n]
  crate_positions[[from]] <- tail(crate_positions[[from]], -n)
  # c(rev(to_move), crate_positions[[to]]) for part 1, everything else is the same
  crate_positions[[to]] <- c(to_move, crate_positions[[to]])
  crate_positions
}

for (i in seq_len(nrow(comms))) {
  crate_positions <<- do_1_move(
    crate_positions,
    comms[i, ]$n,
    comms[i, ]$from,
    comms[i, ]$to
  )
}

paste(sapply(crate_positions, function(col) col[1]), collapse = "")
