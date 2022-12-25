library(tidyverse)
library(glue)
options(scipen = 999)

year <- "2022"
day <- "25"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- read_lines(input_file)

snafu_to_decimal <- function(snafu) {
  snafu_digits <- rev(str_split(snafu, "")[[1]])
  five_base_digits <- match(snafu_digits, c("=", "-", "0", "1", "2")) - 3
  sum(five_base_digits * 5 ^ (0:(length(snafu_digits) - 1)))
}

stopifnot(snafu_to_decimal("2=-01") == 976)

decimal_to_snafu <- function(num) {
  snafu_digits <- list(
    "-2" = "=",
    "-1" = "-",
    "0" = "0",
    "1" = "1",
    "2" = "2"
  )
  digit_value <- if (num %% 5 <= 2) num %% 5 else num %% 5 - 5
  result <- snafu_digits[[as.character(digit_value)]]
  num <- (num - digit_value) / 5
  while (num > 0) {
    digit_value <- if (num %% 5 <= 2) num %% 5 else num %% 5 - 5
    result <- paste0(snafu_digits[[as.character(digit_value)]], result)
    num <- (num - digit_value) / 5
  }
  result
}

stopifnot(decimal_to_snafu(314159265)  == "1121-1110-1=0")

sapply(input, snafu_to_decimal) |> sum() |> decimal_to_snafu()
