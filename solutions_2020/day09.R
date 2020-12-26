library(tidyverse)

input <- read_lines("solutions_2020/day09_input.txt") %>%
  as.numeric()


# part 1 ------------------------------------------------------------------

is_sum <- function(prev_nums, current) {
  sums <- cross_df(list(x = prev_nums, y = prev_nums)) %>%
    filter(x != y) %>%
    mutate(s = x + y) %>%
    pull(s)
  current %in% sums
}

is_correct_at_index <- function(nums, index) {
  is_sum(nums[(index-26):(index-1)], nums[index])
}

current_index <- 26
while(is_correct_at_index(input, current_index)) {
  current_index <<- current_index + 1 
}

input[current_index]


# part 2 ------------------------------------------------------------------


target_num <- 88311122

start_index <- 1
end_index <- 2

while(sum(input[start_index:end_index]) != target_num) {
  while(sum(input[start_index:end_index]) < target_num) {
    end_index <<- end_index + 1
  }
  if (sum(input[start_index:end_index]) != target_num) {
    start_index <<- start_index + 1
    end_index <<- start_index + 1 
  }
}

min(input[start_index:end_index]) + max(input[start_index:end_index])
