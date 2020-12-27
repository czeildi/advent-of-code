library(tidyverse)

range_low <- 246540
range_high <- 787419

valid_passwords <- function(lowest_digit, highest_digit, n_digit, needs_double) {
  never_decrease <- choose(highest_digit - lowest_digit + n_digit, n_digit)
  no_double <- choose(highest_digit - lowest_digit + 1, n_digit)
  if (!needs_double) {
    return(never_decrease)
  }
  never_decrease - no_double
}

all_valid_passwords <- valid_passwords(1, 9, 6, TRUE)

valid_passwords_outside_range <- list(
  starts_with_11 = valid_passwords(1, 9, 4, FALSE),
  starts_with_1b1 = valid_passwords(2, 9, 5, TRUE),
  start_with_22 = valid_passwords(2, 9, 4, FALSE),
  starts_with_233 = valid_passwords(3, 9, 3, FALSE),
  starts_with_23b3 = valid_passwords(4, 9, 4, TRUE),
  starts_with_244 = valid_passwords(4, 9, 3, FALSE),
  starts_with_2455 = valid_passwords(5, 9, 2, FALSE),
  starts_with_245b5 = valid_passwords(6, 9, 3, TRUE),
  start_with_9 = 1,
  starts_with_8 = valid_passwords(8, 9, 5, TRUE),
  starts_with_79 = 1,
  starts_with_78 = valid_passwords(8, 9, 4, TRUE)
) %>% reduce(`+`)

all_valid_passwords - valid_passwords_outside_range
