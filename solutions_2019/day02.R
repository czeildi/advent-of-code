library(tidyverse)

program <- read_lines("solutions_2019/day02_input.txt") %>%
  str_split(",") %>% .[[1]] %>% 
  as.numeric()

for (noun in 0:99) {
  for (verb in 0:99) {
    current_program <- program
    current_program[2] <- noun
    current_program[3] <- verb
    index_to_process <- 1
    
    while(current_program[index_to_process] %in% c(1, 2)) {
      value1 <- current_program[current_program[index_to_process + 1] + 1]
      value2 <- current_program[current_program[index_to_process + 2] + 1]
      result_index <- current_program[index_to_process + 3] + 1
      if (current_program[index_to_process] == 1) {
        op <- `+`
      } else {
        op <- `*`
      }
      current_program[result_index] <- op(value1, value2)
      index_to_process <- index_to_process + 4
    }
    
    if (current_program[1] == 19690720) {
      print(100*noun + verb)
    }
  }
}
