library(tidyverse)

input <- tibble(x = read_lines("solutions_2017/day02_input.txt")) %>% 
  mutate(row_id = 1:n()) %>% 
  separate_rows(x, convert = TRUE) 

# part 1
input %>% 
  group_by(row_id) %>% 
  summarize(diff = max(x) - min(x)) %>% 
  summarize(result = sum(diff))

# part 2

checksum <- function(values) {
  for (a in values) {
    for (b in values) {
      if (a != b && (a / b) == (a %/% b)) {
        return(a/b)
      }
    }
  }
}

input %>% 
  group_by(row_id) %>% 
  summarize(cs = checksum(x)) %>% 
  summarize(result = sum(cs))
