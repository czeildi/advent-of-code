library(tidyverse)

tibble(x = read_lines("solutions_2015/input05.txt")) %>% 
  mutate(
    vowel_count = str_count(x, "[aeiou]"),
    has_repeat = str_detect(x, "([a-z])\\1"),
    has_banned = str_detect(x, "ab|cd|pq|xy")
  ) %>% 
  filter(vowel_count >= 3 & has_repeat & !has_banned) %>% 
  nrow()

tibble(x = read_lines("solutions_2015/input05.txt")) %>% 
  mutate(
    rule_1 = str_detect(x, "([a-z][a-z]).*\\1"),
    rule_2 = str_detect(x, "([a-z])[a-z]\\1")
  ) %>% 
  filter(rule_1 & rule_2) %>% 
  nrow()
