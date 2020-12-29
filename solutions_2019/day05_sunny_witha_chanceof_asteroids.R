library(tidyverse)

program <- read_lines("solutions_2019/day05_input.txt") %>%
  str_split(",") %>%
  .[[1]] %>%
  as.numeric()

commands <- list(
  "1" = list(
    "n_param" = 3,
    "op" = function(raw_params, values) {
      program[raw_params[3] + 1] <<- `+`(values[1], values[2])
    }
  ),
  "2" = list(
    "n_param" = 3,
    "op" = function(raw_params, values) {
      program[raw_params[3] + 1] <<- `*`(values[1], values[2])
    }
  ),
  "3" = list(
    "n_param" = 1,
    "op" = function(raw_params, values) {
      program[raw_params[1] + 1] <<- 1
    }
  ),
 "4" = list(
    "n_param" = 1,
    "op" = function(raw_params, values) {
      print(values[1])
    }
 )
)

idx <- 1

while (program[idx] %% 100 != 99) {
  command <- commands[[as.character(program[idx] %% 100)]]
  parameter_modes <- str_pad(as.character(program[idx] %/% 100), width = command$n_param, pad = "0") %>%
    str_split("") %>%
    .[[1]] %>%
    rev()
  raw_params <- program[idx + 1:command$n_param]
  param_values <- map2_dbl(raw_params, parameter_modes, ~{
    if (.y == "0") program[.x + 1]
    else .x
  })
  command$op(raw_params, param_values)
  idx <- idx + 1 + command$n_param
}
