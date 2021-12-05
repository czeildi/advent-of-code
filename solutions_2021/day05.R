library(tidyverse)

input <- tibble(x = read_lines("solutions_2021/day05_input.txt")) %>% 
  extract(x, c('ax', 'ay', 'bx', 'by'), regex = "(\\d+),(\\d+) -> (\\d+),(\\d+)", convert = TRUE)

gridlines <- input %>% 
  filter(ax == bx | ay == by) 

gridlines %>% 
  summarize(across(everything(), list(min, max)))

add_line <- function(ax, bx, ay, by) {
  if (ax == bx) {
    left <- min(ay, by)
    right <- max(ay, by)
    before <- (ax - 1) * 1000 + left - 1
    len <- right-left + 1
    res <- c(rep(0, before), rep(1, len))
  } else {
    top <- min(ax, bx)
    bottom <- max(ax, bx)
    before <- (top - 1) * 1000 + ay - 1
    len <- bottom - top + 1
    res <- c(rep(0, before), rep(c(1, rep(0, 999)), len - 1), 1)
  }
  return(c(res, rep(0, 10^6 - length(res))))
}

lines <- gridlines %>% 
  rowwise() %>% 
  mutate(line = list(add_line(ax, bx, ay, by))) %>% 
  pull(line)

grid <- reduce(lines, `+`)

length(grid[grid >= 2])
