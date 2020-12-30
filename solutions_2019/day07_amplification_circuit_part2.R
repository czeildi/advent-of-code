library(tidyverse)
library(gtools)

amplifier_program_idx <- 1

read_original_program <- function() {
  read_lines("solutions_2019/day07_input.txt") %>%
    str_split(",") %>%
    .[[1]] %>%
    as.numeric()
}

# to convert to R's 1 based indexing
as_index <- function(x) x + 1

set_value <- function(index, value) {
  amplifier_programs[[amplifier_program_idx]]$program[as_index(index)] <<- value
}

default_step_index <- function(idx, n_param) idx + 1 + n_param

step_program_idx <- function(idx) {
  if (idx == 5) 1
  else idx + 1
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
      if (!amplifier_programs[[amplifier_program_idx]]$first_input_processed) {
        set_value(raw_params[1], amplifier_programs[[amplifier_program_idx]]$amplifier_phase_setting)
        amplifier_programs[[amplifier_program_idx]]$first_input_processed <<- TRUE
      } else if (!is.null(amplifier_programs[[amplifier_program_idx]]$input)){
        set_value(raw_params[1], amplifier_programs[[amplifier_program_idx]]$input)
        amplifier_programs[[amplifier_program_idx]]$input <<- NULL
      } else {
        should_step_program_idx <<- TRUE
      }
    }
  ),
  "4" = list(
    "n_param" = 1,
    "op" = function(raw_params, values) {
      amplifier_programs[[step_program_idx(amplifier_program_idx)]]$input <<- values[1]
      should_step_program_idx <<- TRUE
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

initialize_amplifier_programs <- function(phase_settings) {
  programs <- map(phase_settings, ~{
    list(
      program = read_original_program(),
      input = NULL,
      first_input_processed = FALSE,
      amplifier_phase_setting = .,
      idx = as_index(0)
    )
  })
  programs[[1]]$input <- 0
  programs
}

program_to_process <- function() amplifier_programs[[amplifier_program_idx]]$program
program_idx_to_process <- function() amplifier_programs[[amplifier_program_idx]]$idx
set_program_idx_to_process <- function(value) {
  amplifier_programs[[amplifier_program_idx]]$idx <<- value
}
intcode_to_process <- function() program_to_process()[program_idx_to_process()]

phase_setting_options <- gtools::permutations(5, 5, 5:9)

map_dbl(seq_len(nrow(phase_setting_options)), ~{
  amplifier_programs <<- initialize_amplifier_programs(phase_setting_options[., ])
  amplifier_program_idx <<- 1
  num_halted_amplifiers <<- 0
  should_step_program_idx <<- FALSE

  while (num_halted_amplifiers < 5) {
    while (intcode_to_process() %% 100 != 99) {
      command <- commands[[as.character(intcode_to_process() %% 100)]]
      parameter_modes <- intcode_to_process() %/% 100 %>%
      {str_pad(as.character(.), width = command$n_param, pad = "0")} %>%
        str_split("") %>%
        .[[1]] %>%
        rev()
      raw_params <- program_to_process()[program_idx_to_process() + 1:command$n_param]
      param_values <- map2_dbl(raw_params, parameter_modes, ~{
        if (.y == "0") program_to_process()[as_index(.x)]
        else .x
      })
      if (!is.null(command$op)) {
        command$op(raw_params, param_values)
      }
      if (!is.null(command$step_index)) {
        set_program_idx_to_process(command$step_index(program_idx_to_process(), param_values))
      } else {
        set_program_idx_to_process(default_step_index(program_idx_to_process(), command$n_param))
      }
      if (should_step_program_idx) {
        amplifier_program_idx <<- step_program_idx(amplifier_program_idx)
        should_step_program_idx <<- FALSE
      }
    }
    num_halted_amplifiers <<- num_halted_amplifiers + 1
    amplifier_program_idx <<- step_program_idx(amplifier_program_idx)
  }
  amplifier_programs[[1]]$input # last output of amplifier program No 5
}) %>%
  max()

