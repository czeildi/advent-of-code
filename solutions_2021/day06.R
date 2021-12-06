library(tidyverse)

input <- read_lines("solutions_2021/day06_input.txt") %>% 
  str_split(",") %>% 
  .[[1]] %>% 
  as.numeric()

fish <- function(days, age) {
  if (days == 0) {
    return(1)
  }
  if (age != 0) {
    return(m_fish(days - 1, age - 1))
  }
  return(m_fish_group(days - 1, c(6, 8)))
}

m_fish <- memoise::memoise(fish)

fish_group <- function(days, ages) {
  if (days == 0) {
    return(length(ages))
  }
  tibble::tibble(day = days, age = ages) %>% 
    group_by(age) %>% 
    summarize(n_fish = n(), day = mean(day)) %>% 
    rowwise() %>% 
    mutate(fish_count = m_fish(day, age)) %>% 
    ungroup() %>% 
    summarize(res = sum(fish_count * n_fish)) %>% 
    pull(res)
}

m_fish_group <- memoise::memoise(fish_group)

system.time(
  result <- m_fish_group(256, input)
)

as.character(result)