max_len <- 30000000
starting_numbers <- c(9, 3, 1, 0, 8, 4)

## initialization
latest_indices <- rep(NA_integer_, max_len)
# add 1 so that 0 has a place as well
purrr::iwalk(head(starting_numbers, -1), ~{
  latest_indices[.x + 1] <<- .y
})
previous_number <- tail(starting_numbers, 1)

## calculation
do_step <- function(previous_number, current_index) {
  prev_occurence <- latest_indices[previous_number + 1]
  latest_indices[previous_number + 1] <<- current_index - 1
  if(is.na(prev_occurence)) {
    return(0)
  } else {
    return(current_index - 1 - prev_occurence)
  }
}

system.time({
  for (current_index in (length(starting_numbers) + 1):max_len) {
    previous_number <<- do_step(previous_number, current_index)
  }
})

previous_number

## measurements ------------------------------------------------------
# 10.000 0.01 sec
# 100.000 0.156sec
# 1.000.000 1.5sec
