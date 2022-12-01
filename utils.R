read_as_grid <- function(input, col_sep = "") {
  tibble(value = input) %>%
    separate_rows(value, sep = "\n") %>%
    mutate(y = 1:n()) %>%
    separate_rows(value, sep = col_sep) %>%
    filter(value != "") %>%
    group_by(y) %>%
    mutate(x = 1:n()) %>%
    ungroup()
}

one_indexed_remainder <- function(value, mod) {
  if_else(value %% mod == 0, mod, value %% mod)
}

directions <- tribble(
  ~direction, ~dx, ~dy,
  "tl", -1, -1,
  "t", 0, -1,
  "tr", 1, -1,
  "l", -1, 0,
  "self", 0, 0,
  "r", 1, 0,
  "bl", -1, 1,
  "b", 0, 1,
  "br", 1, 1
) %>%
  select(-direction)

binary_vec_to_decimal <- function(binary) {
  sum(rev(binary) * 2 ^ (0:(length(binary) - 1)))
}

decimal_to_n_digit_binary <- function(num, n) {
  result <- num %% 2
  num <- (num - result) / 2
  while (num > 0) {
    result <- paste0(num %% 2, result)
    num <- (num - num %% 2) / 2
  }
  str_pad(result, n, pad = "0")
}

sort_chars <- function(word) {
  paste(sort(str_split(word, "")[[1]]), collapse = "")
}
