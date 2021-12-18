library(tidyverse)
library(zeallot)
library(stringi)

explode <- function(input, position_for_explode) {
  before <- str_sub(input, 1, position_for_explode - 1)
  rest <- str_sub(input, position_for_explode)
  exploding_pair <- str_extract(rest, "\\[\\d+,\\d+\\]")
  after <- str_sub(rest, nchar(exploding_pair) + 1)
  c(first, second) %<-% unglue::unglue_data(exploding_pair, "[{first},{second}]", convert = TRUE)
  
  if (str_detect(after, "\\d+")) {
    right <- str_extract(after, "\\d+")
    new_right <- as.character(as.numeric(right) + second)
    new_after <- str_replace(after, "\\d+", new_right)
  } else {
    new_after <- after
  }
  
  if (str_detect(before, "\\d+")) {
    reversed_before <- stringi::stri_reverse(before)
    left <- str_extract(reversed_before, "\\d+")
    new_left <- stri_reverse(as.character(as.numeric(stri_reverse(left)) + first))
    new_before_reversed <- str_replace(reversed_before, "\\d+", new_left)
    new_before <- stri_reverse(new_before_reversed)
  } else {
    new_before <- before
  }
  
  after_explode <- paste0(new_before, "0", new_after)
}

split <- function(input, to_split) {
  num <- as.numeric(to_split)
  left <- num %/% 2
  right <- num - left
  replacement <- paste0("[", left, ",", right, "]")
  str_replace(input, to_split, replacement)
}

reduce_input <- function(input) {
  position_for_explode <- map_chr(seq_len(nchar(input)), ~str_sub(input, 1, .x)) %>% 
    keep(~{ str_count(., "\\[") - str_count(., "\\]") >= 5 }) %>% 
    pluck(1) %>% 
    nchar()
  
  if (length(position_for_explode) == 1) {
    return(reduce_input(explode(input, position_for_explode)))
  }
  
  to_split <- str_extract(input, "\\d{2,}")
  if (!is.na(to_split)) {
    return(reduce_input(split(input, to_split)))
  }
  return(input)
}

add_snailfish <- function(p1, p2) {
  paste0("[", p1, ",", p2, "]")
}

fish_numbers <- read_lines("solutions_2021/input18.txt")

# part 1
final_sum <- reduce(fish_numbers, function(prev, current) {
  reduce_input(add_snailfish(prev, current))
})

write_lines(final_sum, "solutions_2021/day18_intermediate_p1.txt")

# magnitude calculated in javascript with  "node solutions_2021/day18.js"

# part 2
candidates <- crossing(p1 = fish_numbers, p2 = fish_numbers) %>% 
  filter(p1 != p2) %>% 
  mutate(idx = 1:n())

candidate_sums <- pmap_chr(candidates, ~{
  print(..3)
  system.time(
    res <- reduce_input(add_snailfish(..1, ..2))
  )
  res
})

write_lines(candidate_sums, "solutions_2021/day18_intermediate.txt")

# max magnitude calculated in javascript with "node solutions_2021/day18.js"