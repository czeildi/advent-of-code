library(tidyverse)
library(glue)

input <- read_lines("solutions_2021/day10_input.txt")

corruption_values <- c(")" = 3, "]" = 57, "}" = 1197, ">" = 25137)

opening <- c("(", "[", "{", "<")
closing <- c(")", "]", "}", ">")

opening_matches <- opening
names(opening_matches) <- closing

is_match <- function(verem, current) {
  if (length(verem) == 0) return (FALSE)
  current %in% closing && tail(verem, 1) == opening_matches[current]
}

is_corrupt <- function(verem, current) {
  if (length(verem) == 0) return(FALSE)
  current %in% closing && opening_matches[current] != tail(verem, 1)
}

corruption_value <- function(line) {
  chars <- line %>% str_split("") %>% .[[1]]
  
  verem <- c()
  next_char_idx <- 1
  
  while(next_char_idx <= length(chars) && !is_corrupt(verem, chars[next_char_idx])) {
    if (is_match(verem, chars[next_char_idx])) {
      verem <- head(verem, length(verem) - 1)
    } else {
      verem <- c(verem, chars[next_char_idx])
    }
    
    next_char_idx <- next_char_idx + 1
  }
  
  if(!next_char_idx > length(chars)) {
    value <- corruption_values[chars[next_char_idx]]
  } else {
    value <- 0
  }
  
  return(value)
}

map_dbl(input, corruption_value) %>% sum()

# part 2

completion_values <- c("(" = 1, "[" = 2, "{" = 3, "<" = 4)

incomplete_lines <- keep(input, ~{corruption_value(.x) == 0})

completion_value <- function(line) {
  chars <- line %>% str_split("") %>% .[[1]]
  
  verem <- c()
  next_char_idx <- 1
  
  while(next_char_idx <= length(chars)) {
    if (is_match(verem, chars[next_char_idx])) {
      verem <- head(verem, length(verem) - 1)
    } else {
      verem <- c(verem, chars[next_char_idx])
    }
    
    next_char_idx <- next_char_idx + 1
  }
  
  reduce(rev(verem), function(prev, current) {
    return(prev * 5 + completion_values[current])
  }, .init = 0)
}

map_dbl(incomplete_lines, completion_value) %>% median()
