library(tidyverse)

program <- read_lines("solutions_2019/day02_input.txt") %>%
  str_split(",") %>% .[[1]] %>% 
  as.numeric()

program[2] <- 12
program[3] <- 2

index_to_process <- 1

while(program[index_to_process] %in% c(1, 2)) {
  value1 <- program[program[index_to_process + 1] + 1]
  value2 <- program[program[index_to_process + 2] + 1]
  result_index <- program[index_to_process + 3] + 1
  if (program[index_to_process] == 1) {
    op <- `+`
  } else {
    op <- `*`
  }
  program[result_index] <- op(value1, value2)
  index_to_process <- index_to_process + 4
}

program[1]