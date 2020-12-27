library(tidyverse)

range_low <- 246540
range_high <- 787419

non_decreasing <- function(min_digit, max_digit, n_digit) {
  map(min_digit:max_digit, ~{
    if (n_digit == 1) return(.x)
    .x * 10^(n_digit - 1) + non_decreasing(.x, max_digit, n_digit - 1)
  }) %>% 
    unlist()
}

non_decreasing_in_range <- non_decreasing(1, 9, 6) %>% 
  .[. >= range_low] %>% 
  .[. <= range_high]

# part 1
keep(non_decreasing_in_range, function(password) {
  length(table(str_split(password, "")) %>% .[. >= 2]) >= 1
}) %>% 
  length()

# part 2
keep(non_decreasing_in_range, function(password) {
  length(table(str_split(password, "")) %>% .[. == 2]) >= 1
}) %>% 
  length()
