library(tidyverse)

max_len <- 30000000
starting_numbers <- c(9, 3, 1, 0, 8, 4)
latest_indices <- rep(NA_integer_, max_len)
latest_indices[10] <- 1
latest_indices[4] <- 2
latest_indices[2] <- 3
latest_indices[1] <- 4
latest_indices[9] <- 5

do_step <- function(previous_number, current_index) {
  prev_occurence <- latest_indices[previous_number + 1]
  if(is.na(prev_occurence)) {
    res <- 0
  } else {
    res <- current_index - 1 - prev_occurence
  }
  latest_indices[previous_number + 1] <<- current_index - 1
  return(res)
}

current_index <- length(starting_numbers) + 1
previous_number <- tail(starting_numbers, 1)
system.time({
  while(current_index <= max_len) {
    previous_number <<- do_step(previous_number, current_index)
    current_index <<- current_index + 1
  }
})

previous_number

# 10.000 0.01 sec
# 100.000 0.156sec
# 1.000.000 1.5sec
