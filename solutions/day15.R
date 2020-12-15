library(tidyverse)

max_len <- 100000
starting_numbers <- c(9, 3, 1, 0, 8, 4)
numbers <- c(starting_numbers, rep(NA_integer_, max_len))

latest_zero_index <- 4
next_number <- function(current_index) {
  last <- numbers[current_index - 1]
  if (last == 0) {
    res <- current_index - 1 - latest_zero_index
    latest_zero_index <<- (current_index - 1)
    return(res)
  }
  indices <- which(numbers == last)
  if (length(indices) == 1) {
    return(0)
  } else {
    return(rev(indices)[1] - rev(indices)[2])
  }
}

current_index <- 7
system.time({
  while(current_index <= max_len) {
    numbers[current_index] <- next_number(current_index)
    current_index <<- current_index + 1
  }
})

numbers[current_index - 1]

# 10.000 0.4 sec
# 25.000 2.5 sec
# 50.000 9 sec
# 100.000 30sec
