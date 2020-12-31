library(tidyverse)

program <- read_lines("solutions_2019/day09_input.txt") %>%
  str_split(",") %>%
  .[[1]] %>%
  as.numeric()

PROGRAM_INPUT <- 2
PROGRAM_OUTPUT <- NULL
RELATIVE_BASE <- 0

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
    "op" = function(params_as_positions, values) {
      set_value(params_as_positions[3], `+`(values[1], values[2]))
    }
  ),
  "2" = list(
    "n_param" = 3,
    "op" = function(params_as_positions, values) {
      set_value(params_as_positions[3], `*`(values[1], values[2]))
    }
  ),
  "3" = list(
    "n_param" = 1,
    "op" = function(params_as_positions, values) {
      set_value(params_as_positions[1], PROGRAM_INPUT)
    }
  ),
  "4" = list(
    "n_param" = 1,
    "op" = function(params_as_positions, values) {
      PROGRAM_OUTPUT <<- values[1]
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
    "op" = function(params_as_positions, values) {
      condition <- values[1] < values[2]
      set_value(params_as_positions[3], if_else(condition, 1, 0))
    }
  ),
  "8" = list(
    "n_param" = 3,
    "op" = function(params_as_positions, values) {
      condition <- values[1] == values[2]
      set_value(params_as_positions[3], if_else(condition, 1, 0))
    }
  ),
  "9" = list(
    "n_param" = 1,
    "op" = function(params_as_positions, values) {
      RELATIVE_BASE <<- RELATIVE_BASE + values[1]
    }
  )
)

read_program_memory <- function(indices) {
  replace_na(program[indices], 0)
}

idx <- as_index(0)

while (program[idx] %% 100 != 99) {
  command <- commands[[as.character(program[idx] %% 100)]]
  parameter_modes <- program[idx] %/% 100 %>%
  {str_pad(as.character(.), width = command$n_param, pad = "0")} %>%
    str_split("") %>%
    .[[1]] %>%
    rev()
  raw_params <- read_program_memory(idx + 1:command$n_param)
  params_as_positions <- map2_dbl(raw_params, parameter_modes, ~{
    if (.y == "2") RELATIVE_BASE + .x
    else .x
  })
  param_values <- map2_dbl(params_as_positions, parameter_modes, ~{
    if (.y == "1") .x
    else read_program_memory(as_index(.x))
  })
  if (!is.null(command$op)) {
    command$op(params_as_positions, param_values)
  }
  if (!is.null(command$step_index)) {
    idx <- command$step_index(idx, param_values)
  } else {
    idx <- default_step_index(idx, command$n_param)
  }
}

PROGRAM_OUTPUT
