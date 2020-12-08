library(tidyverse)

instructions <- tibble(x = read_lines("solutions/day08_input.txt")) %>%
  separate(x, c('command', 'num'), convert = TRUE, sep = " ") %>%
  mutate(row_n = seq_len(nrow(.)))


# part 1 ------------------------------------------------------------------

visit_nums <- rep(0, nrow(instructions))

global_acc <- 0
current_row_index <- 1

while(visit_nums[current_row_index] == 0) {
  visit_nums[current_row_index] <- 1
  command <- instructions %>% 
    filter(row_n == current_row_index) %>%
    pull(command)
  num <- instructions %>% 
    filter(row_n == current_row_index) %>%
    pull(num)
  if (command == 'acc') {
    current_row_index <- current_row_index + 1
    global_acc <- global_acc + num
  } else if (command == 'jmp') {
    current_row_index <- current_row_index + num
  } else {
    current_row_index <- current_row_index + 1
  }
}

print(global_acc)


# part 2 ------------------------------------------------------------------

changed_row_index <- 1
visit_nums <- rep(0, nrow(instructions) + 1)
global_acc <- 0
current_row_index <- 1

while (current_row_index != (nrow(instructions) + 1)) {
  print("index:")
  print(changed_row_index)
  global_acc <<- 0
  current_row_index <<- 1
  old_command <- instructions %>%
    filter(row_n == changed_row_index) %>%
    pull(command)
  if (old_command == 'jmp') {
    new_command <- 'nop'
  } else if (old_command == 'nop') {
    new_command <- 'jmp'
  } else {
    new_command <- 'acc'
  }
  changed_instructions <- instructions %>%
    mutate(command = if_else(row_n == changed_row_index, new_command, command))
  while(visit_nums[current_row_index] == 0 && current_row_index != (nrow(changed_instructions) + 1)) {
    visit_nums[current_row_index] <- 1
    command <- changed_instructions %>% 
      filter(row_n == current_row_index) %>%
      pull(command)
    num <- changed_instructions %>% 
      filter(row_n == current_row_index) %>%
      pull(num)
    # cat(current_row_index, command, num, '\n')
    if (command == 'acc') {
      current_row_index <<- current_row_index + 1
      global_acc <<- global_acc + num
    } else if (command == 'jmp') {
      current_row_index <<- current_row_index + num
    } else {
      current_row_index <<- current_row_index + 1
    }
  }
  visit_nums <<- rep(0, nrow(instructions) + 1)
  changed_row_index <<- changed_row_index + 1
}

print(global_acc)


