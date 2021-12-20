library(tidyverse)

read_lines("solutions_2015/input12.txt") %>% 
  str_extract_all("[-]?\\d+") %>% 
  .[[1]] %>% 
  as.numeric() %>% 
  sum()

input <- jsonlite::fromJSON(read_lines("solutions_2015/input12.txt"))
