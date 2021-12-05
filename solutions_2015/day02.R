library(tidyverse)

paper_need <- function(h, w, l) {
  2 * (w * l + w * h + h * l) + min(w * l, w * h, h * l)
}

ribbon_need <- function(h, w, l) {
  2 * (w + h + l) - 2 * max(w, h, l) + w * h * l
}

tibble(x = read_lines("solutions_2015/input02.txt")) %>% 
  separate(x, c("h", "w", "l"), sep = "x", convert = TRUE) %>% 
  rowwise() %>% 
  mutate(paper = paper_need(h, w, l), ribbon = ribbon_need(h, w, l)) %>% 
  ungroup() %>% 
  summarize(result1 = sum(paper), result2 = sum(ribbon))

