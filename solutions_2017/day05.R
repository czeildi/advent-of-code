library(tidyverse)

jumps <- as.numeric(read_lines("solutions_2017/input05.txt"))

n_step <- 0
idx <- 1

while(idx >= 1 && idx <= length(jumps)) {
  offset <- jumps[idx]
  if (offset >= 3) {
    jumps[idx] <- jumps[idx] - 1
  } else {
    jumps[idx] <- jumps[idx] + 1
  }
  n_step <- n_step + 1
  idx <- idx + offset
}

n_step