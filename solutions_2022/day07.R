library(tidyverse)
library(glue)

year <- "2022"
day <- "07"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- read_lines(input_file)
# hack to make sure we go back to the root
input <- c(input, rep("$ cd ..", 50))

dirs <- list(
  "/" = list(size = 0, parent = NA)
)

current_dir <- "/"

# solution assumes optiomal travelsal: no dir visited twice or ls-d twice
for (cmd_line in tail(input, -1)) {
  if (str_detect(cmd_line, "^\\$ cd")) {
    next_dir <- str_match(cmd_line, "^\\$ cd (.*)")[1, 2]

    if (next_dir == "..") {
      next_dir <- dirs[[current_dir]]$parent
      if (is.na(next_dir)) break() # stop if we attempt to go above root in the end
      dirs[[next_dir]]$size <- dirs[[next_dir]]$size + dirs[[current_dir]]$size
      current_dir <- next_dir
    } else {
      # hack to deal with duplicate folder names at different paths
      next_dir <- paste0(current_dir, "/", next_dir)
      dirs[[next_dir]] <- list(size = 0, parent = current_dir)
      current_dir <- next_dir
    }
  }
  if (str_detect(cmd_line, "^(\\d+)")) {
    file_size <- as.numeric(str_match(cmd_line, "^(\\d+)")[1, 2])
    dirs[[current_dir]]$size <- dirs[[current_dir]]$size + file_size
  }
}

to_free <- dirs[["/"]]$size - 4 * 10^7

# part 1
sapply(dirs, \(d) if (d$size <= 100000) d$size else 0) |> sum()

# part 2
sapply(dirs, \(d) if (d$size >= to_free) d$size else Inf) |> min()
