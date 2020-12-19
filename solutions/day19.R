library(tidyverse)

input <- read_file("solutions/day19_input.txt") %>% 
  str_split("\n\n") %>% .[[1]]

rules <- tibble(x = input[1]) %>%
  separate_rows(x, sep = "\n") %>%
  extract(x, c('rule_idx', 'rule'), regex = "(\\d+): \"?([0-9ab |]+)", convert = TRUE) %>%
  arrange(rule_idx) %>%
  deframe()

rule_for_alternative <- function(rule) {
  parts <- str_split(rule, " ") %>% .[[1]] %>% as.integer()
  parts %>% 
    map_chr(m_rule_regex) %>% 
    map(~str_c("(", .x, ")")) %>%
    str_c(collapse = "")
}

rule_regex <- function(rule_idx) {
  rule <- rules[rule_idx + 1]
  if (rule == "a" || rule == "b") return (rule)
  alternatives <- str_split(rule, " [|] ") %>% .[[1]]
  if (length(alternatives) == 1) {
    new_rule <- rule_for_alternative(alternatives[1])
  } else {
    new_rule <- alternatives %>%
      map_chr(rule_for_alternative) %>%
      map(~str_c("(", .x, ")")) %>%
      str_c(collapse = "|") 
  }
  if (rule_idx == 0) {
    return(str_c("^", new_rule, "$"))
  } else {
    return(new_rule)
  }
}

m_rule_regex <- memoise::memoise(rule_regex)

full_regex <- m_rule_regex(0)

input[2] %>% str_split('\n') %>% .[[1]] %>%
  map_lgl(~str_detect(., full_regex)) %>%
  sum()
  