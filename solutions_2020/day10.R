library(tidyverse)

jolts <- as.numeric(read_lines("solutions_2020/day10_input.txt")) %>%
  sort()


# part 1 ------------------------------------------------------------------

diffs <- tibble(j = c(0, jolts, max(jolts) + 3)) %>%
  mutate(diff = j - lag(j))

diffs %>% count(diff)


# part 2 ------------------------------------------------------------------

res <- tibble(j = c(0, jolts, max(jolts) + 3)) %>%
  mutate(diff = j - lag(j)) %>%
  mutate(ends_with = 1, ends_with_1_less = 0, ends_with_2_less = 0) %>%
  purrr::transpose()

for (current_index in 2:length(res)) {
  prev_row <- res[[current_index - 1]]
  diff <- res[[current_index]]$diff
  if (diff == 1) {
    res[[current_index]]$ends_with <- prev_row$ends_with + prev_row$ends_with_1_less + prev_row$ends_with_2_less
    res[[current_index]]$ends_with_1_less <- prev_row$ends_with
    res[[current_index]]$ends_with_2_less <- prev_row$ends_with_1_less
  }
  if (diff == 2) {
    res[[current_index]]$ends_with <- prev_row$ends_with + prev_row$ends_with_1_less
    res[[current_index]]$ends_with_1_less <- 0
    res[[current_index]]$ends_with_2_less <- prev_row$ends_with
  }
  if (diff == 3) {
    res[[current_index]]$ends_with <- prev_row$ends_with
    res[[current_index]]$ends_with_1_less <- 0
    res[[current_index]]$ends_with_2_less <- 0
  }
}

res[[length(res)]]$ends_with
sprintf("%.0f",res[[length(res)]]$ends_with)
