library(tidyverse)

# part 1 ---------------------------

tibble(x = read_lines("solutions_2021/day08_input.txt")) %>% 
  separate(x, into = c("input", "output"), sep = " \\| ") %>% 
  select(output) %>% 
  separate_rows(output) %>% 
  mutate(x = nchar(output)) %>% 
  filter(x == 2 | x == 4 | x == 3 | x == 7)

# part 2 ------------------------------

is_part_of <- function(s1, s2) {
  chars1 <- str_split(s1, "")[[1]]
  chars2 <- str_split(s2, "")[[1]]
  all(chars1 %in% chars2)
}

size_of_intersection <- function(s1, s2) {
  chars1 <- str_split(s1, "")[[1]]
  chars2 <- str_split(s2, "")[[1]]
  length(intersect(chars1, chars2))
}

sort_chars <- function(s1) {
  paste(sort(str_split(s1, "")[[1]]), collapse = "")
}

digits_in_order <- function(input) {
  digits <- str_split(input, " ")[[1]]
  
  d1 <- digits[nchar(digits) == 2]
  digits <- setdiff(digits, d1)
  
  d4 <- digits[nchar(digits) == 4]
  digits <- setdiff(digits, d4)
  
  d7 <- digits[nchar(digits) == 3]
  digits <- setdiff(digits, d7)
  
  d8 <- digits[nchar(digits) == 7]
  digits <- setdiff(digits, d8)
  
  d9 <- keep(digits, ~nchar(.x) == 6 && is_part_of(d4, .x))
  digits <- setdiff(digits, d9)
  
  d0 <- keep(digits, ~nchar(.x) == 6 && is_part_of(d1, .x))
  digits <- setdiff(digits, d0)
  
  d6 <- digits[nchar(digits) == 6]
  digits <- setdiff(digits, d6)
  
  d3 <- keep(digits, ~nchar(.x) == 5 && is_part_of(d1, .x))
  digits <- setdiff(digits, d3)
  
  d2 <- keep(digits, ~nchar(.x) == 5 && size_of_intersection(.x, d4) == 2)
  digits <- setdiff(digits, d2)
  
  d5 <- digits
  
  in_order <- c(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9)
  sorted_digits <- map_chr(in_order, sort_chars)
  result <- 0:9
  names(result) <- sorted_digits
  result
}

output_value <- function(digits, output) {
  str_split(output, " ") %>% 
    .[[1]] %>% 
    map_chr(sort_chars) %>% 
    {as.numeric(paste(digits[.], collapse = ""))}
}

tibble(x = read_lines("solutions_2021/day08_input.txt")) %>% 
  separate(x, into = c("input", "output"), sep = " \\| ") %>% 
  rowwise() %>% 
  mutate(result = output_value(digits_in_order(input), output)) %>% 
  ungroup() %>% 
  summarize(res = sum(result))
