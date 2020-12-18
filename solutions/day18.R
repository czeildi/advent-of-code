library(tidyverse)

input <- read_lines("solutions/day18_input.txt")

leftmost_calculation_replaced <- function(expression) {
  to_eval <- str_extract(expression, "[0-9]+ [*+] [0-9]+")
  result <- eval(parse(text = to_eval))
  str_replace(expression, fixed(to_eval), result)
}

calculate <- function(expression) {
  suppressWarnings(numeric_result <- as.numeric(expression))
  if (!is.na(numeric_result)) return(numeric_result)
  
  first_inner_paranthesis <- str_extract(expression, "\\([0-9 *+]+\\)")
  
  if (!is.na(first_inner_paranthesis)) {
    simplified_inner <- leftmost_calculation_replaced(first_inner_paranthesis)
    simplified <- str_replace(expression, fixed(first_inner_paranthesis), simplified_inner)
  } else {
    simplified <- leftmost_calculation_replaced(expression)
  }
  
  without_dummy_paranthesis <- str_replace_all(simplified, "\\(([0-9]+)\\)", "\\1")
  return(calculate(without_dummy_paranthesis))
}

map_dbl(input, calculate) %>% sum() %>% sprintf("%.0f", .)
