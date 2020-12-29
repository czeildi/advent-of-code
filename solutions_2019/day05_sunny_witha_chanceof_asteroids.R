library(tidyverse)

program <- read_lines("solutions_2019/day05_input.txt") %>%
  str_split(",") %>%
  .[[1]] %>%
  as.numeric()

PROGRAM_INPUT <- 5

# to convert to R's 1 based indexing
as_index <- function(x) x + 1

set_value <- function(index, value) {
  program[as_index(index)] <<- value
}

default_step_index <- function(idx, n_param) {
  idx + 1 + n_param
}

commands <- list(
  "1" = list(
    "n_param" = 3,
    "op" = function(raw_params, values) {
      set_value(raw_params[3], `+`(values[1], values[2]))
    }
  ),
  "2" = list(
    "n_param" = 3,
    "op" = function(raw_params, values) {
      set_value(raw_params[3], `*`(values[1], values[2]))
    }
  ),
  "3" = list(
    "n_param" = 1,
    "op" = function(raw_params, values) {
      set_value(raw_params[1], PROGRAM_INPUT)
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
    "step_index" = function(idx, values) {
      if (values[1] != 0) as_index(values[2])
      else default_step_index(idx, length(values))
    }
  ),
  "6" = list(
    "n_param" = 2,
    "step_index" = function(idx, values) {
      if (values[1] == 0) as_index(values[2])
      else default_step_index(idx, length(values))
    }
  ),
  "7" = list(
    "n_param" = 3,
    "op" = function(raw_params, values) {
      condition <- values[1] < values[2]
      set_value(raw_params[3], if_else(condition, 1, 0))
    }
  ),
  "8" = list(
    "n_param" = 3,
    "op" = function(raw_params, values) {
      condition <- values[1] == values[2]
      set_value(raw_params[3], if_else(condition, 1, 0))
    }
  )
)

idx <- as_index(0)

while (program[idx] %% 100 != 99) {
  command <- commands[[as.character(program[idx] %% 100)]]
  parameter_modes <- program[idx] %/% 100 %>%
    {str_pad(as.character(.), width = command$n_param, pad = "0")} %>%
    str_split("") %>%
    .[[1]] %>%
    rev()
  raw_params <- program[idx + 1:command$n_param]
  param_values <- map2_dbl(raw_params, parameter_modes, ~{
    if (.y == "0") program[as_index(.x)]
    else .x
  })
  if (!is.null(command$op)) {
    command$op(raw_params, param_values)
  }
  if (!is.null(command$step_index)) {
    idx <- command$step_index(idx, param_values)
  } else {
    idx <- default_step_index(idx, command$n_param)
  }
}
