library(tidyverse)
library(gtools)

# global variables
amplifier_input <- NULL
amplifier_output <- NULL
program <- NULL
amplifier_phase_setting <- NULL
first_input_processed <- NULL


read_original_program <- function() {
  read_lines("solutions_2019/day07_input.txt") %>%
    str_split(",") %>%
    .[[1]] %>%
    as.numeric()
}

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
      if (!first_input_processed) {
        set_value(raw_params[1], amplifier_phase_setting)
        first_input_processed <<- TRUE
      } else {
        set_value(raw_params[1], amplifier_input)
      }
    }
  ),
  "4" = list(
    "n_param" = 1,
    "op" = function(raw_params, values) {
      amplifier_output <<- values[1]
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

phase_setting_options <- gtools::permutations(5, 5, 0:4)

map_dbl(seq_len(nrow(phase_setting_options)), ~{
  phase_settings <- phase_setting_options[., ]

  amplifier_input <<- 0
  amplifier_output <<- NULL

  for (amplifier_idx in 1:5) {
    program <<- read_original_program()
    amplifier_phase_setting <<- phase_settings[amplifier_idx]
    idx <- as_index(0)
    first_input_processed <<- FALSE

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
    amplifier_input <<- amplifier_output
  }
  amplifier_output
}) %>%
  max()

