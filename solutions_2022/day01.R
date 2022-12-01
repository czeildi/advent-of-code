library(tidyverse)

input <- read_file("solutions_2022/day01_input.txt")

# part 1
sapply(str_split(input, "\n\n")[[1]], function(elf) {
  sum(as.integer(str_split(elf, "\n")[[1]]), na.rm = TRUE)
}) |> max()

# part 2
sapply(str_split(input, "\n\n")[[1]], function(elf) {
  sum(as.integer(str_split(elf, "\n")[[1]]), na.rm = TRUE)
}) |> sort() |> tail(3) |> sum()
