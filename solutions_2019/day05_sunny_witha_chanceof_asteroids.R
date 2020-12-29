library(tidyverse)

program <- read_lines("solutions_2019/day05_input.txt") %>%
  str_split(",") %>%
  .[[1]] %>%
  as.numeric()

PROGRAM_INPUT <- 5

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
      program[raw_params[1] + 1] <<- PROGRAM_INPUT
    }
  ),
  "4" = list(
    "n_param" = 1,
    "op" = function(raw_params, values) {
      print(values[1])
    }
  ),
  "5" = list(
    "n_param" = 2,
    "op" = function(raw_params, values) { },
    "step_index" = function(raw_params, values) {
      if (values[1] != 0) values[2] + 1
      else NULL
    }
  ),
  "6" = list(
    "n_param" = 2,
    "op" = function(raw_params, values) { },
    "step_index" = function(raw_params, values) {
      if (values[1] == 0) values[2] + 1
      else NULL
    }
  ),
  "7" = list(
    "n_param" = 3,
    "op" = function(raw_params, values) {
      comparison_result <- if_else(values[1] < values[2], 1, 0)
      program[raw_params[3] + 1] <<- comparison_result
    }
  ),
  "8" = list(
    "n_param" = 3,
    "op" = function(raw_params, values) {
      comparison_result <- if_else(values[1] == values[2], 1, 0)
      program[raw_params[3] + 1] <<- comparison_result
    }
  )
)

default_step_index <- function(idx, n_param) {
  idx + 1 + n_param
}

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
  if (!is.null(command$step_index)) {
    potential_new_idx <- command$step_index(raw_params, param_values)
    if (is.null(potential_new_idx)) {
      idx <- default_step_index(idx, command$n_param)
    } else {
      idx <- potential_new_idx
    }
  } else {
    idx <- default_step_index(idx, command$n_param)
  }
}
