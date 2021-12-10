library(tidyverse)
library(unglue)

instructions <- read_lines("solutions_2015/input07.txt")

BIT_SIZE <- 16

binary_vec_to_decimal <- function(binary_digits) {
  sum(rev(binary_digits) * (2 ^ (0:(BIT_SIZE - 1))))
}

decimal_to_binary <- function(num) {
  result <- num %% 2
  num <- (num - result) / 2
  while (num > 0) {
    result <- c(num %% 2, result)
    num <- (num - num %% 2) / 2
  }
  c(rep(0, BIT_SIZE - length(result)), result)
}

shift_l <- function(num, n) {
  binary_digits <- decimal_to_binary(num)
  shifted <- c(binary_digits[(n+1):BIT_SIZE], binary_digits[1:n])
  binary_vec_to_decimal(shifted)
}

shift_r <- function(num, n) {
  binary_digits <- decimal_to_binary(num)
  shifted <- c(binary_digits[(BIT_SIZE - n + 1):BIT_SIZE], binary_digits[1:(BIT_SIZE - n)])
  binary_vec_to_decimal(shifted)
}

print_stuff <- function(...) {
  # print(paste(..., sep = ";"))
}


wires <- list()

apply_instruction <- function(instruction) {
  if (str_detect(instruction, "^\\d+ ->")) {
    parts <- unglue_data(instruction, "{value} -> {var}", convert = TRUE)
    wires[[parts$var]] <<- parts$value
    return(TRUE)
  } else if (str_detect(instruction, "^[a-z]+ ->")) {
      parts <- unglue_data(instruction, "{var_1} -> {var}", convert = TRUE)
      if (parts$var_1 %in% names(wires)) {
        wires[[parts$var]] <<- wires[[parts$var_1]]
        print_stuff(instruction)
        return(TRUE)
      }
  } else if (str_detect(instruction, "^NOT [a-z]+ ->")) {
    parts <- unglue_data(instruction, "NOT {var_1} -> {var}", convert = TRUE)
    if (parts$var_1 %in% names(wires)) {
      wires[[parts$var]] <<- 2^BIT_SIZE - 1 - wires[[parts$var_1]]
      print_stuff(instruction)
      return(TRUE)
    }
  } else if (str_detect(instruction, "^[a-z]+ [A-Z]+ [a-z]+ ->")) {
    parts <- unglue_data(instruction, "{var_1} {op} {var_2} -> {var}", convert = TRUE)
    if (parts$var_1 %in% names(wires) && parts$var_2 %in% names(wires)) {
      if (parts$op == 'AND') {
        bitwFun <<- bitwAnd
      } else {
        bitwFun <<- bitwOr
      }
      wires[[parts$var]] <<- bitwFun(wires[[parts$var_1]], wires[[parts$var_2]])
      print_stuff(instruction)
      return(TRUE)
    }
  } else if (str_detect(instruction, "^[0-9]+ [A-Z]+ [a-z]+ ->")) {
    parts <- unglue_data(instruction, "{val} {op} {var_1} -> {var}", convert = TRUE)
    if (parts$var_1 %in% names(wires)) {
      if (parts$op == 'AND') {
        bitwFun <<- bitwAnd
      } else {
        bitwFun <<- bitwOr
      }
      wires[[parts$var]] <<- bitwFun(parts$val, wires[[parts$var_1]])
      print_stuff(instruction)
      return(TRUE)
    }
  } else if (str_detect(instruction, "^[a-z]+ [A-Z]+ \\d+ ->")) {
    parts <- unglue_data(instruction, "{var_1} {op} {value} -> {var}", convert = TRUE)
    if (parts$var_1 %in% names(wires)) {
      if (parts$op == 'RSHIFT') {
        bitwFun <<- shift_r
      } else {
        bitwFun <<- shift_l
      }
      wires[[parts$var]] <<- bitwFun(wires[[parts$var_1]], parts$value)
      print_stuff(instruction)
      return(TRUE)
    }
  }
  return(FALSE)
}

while (!"a" %in% names(wires) && length(instructions) > 0) {
  instructions <<- discard(instructions, apply_instruction)
}

wires[["a"]]