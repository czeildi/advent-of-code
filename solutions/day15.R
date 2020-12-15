library(tidyverse)

numbers <- c(9, 3, 1, 0, 8, 4)

next_number <- function(previous_numbers) {
  last <- tail(previous_numbers, 1)
  indices <- which(previous_numbers == last)
  if (length(indices) == 1) {
    return(0)
  } else {
    return(rev(indices)[1] - rev(indices)[2])
  }
}

while(length(numbers) != 2020) {
  numbers <<- c(numbers, next_number(numbers))
}

tail(numbers, 1)