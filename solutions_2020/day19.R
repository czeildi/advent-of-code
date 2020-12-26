library(tidyverse)

input <- read_file("solutions_2020/day19_input.txt") %>% 
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
  } else if (rule_idx == 8) {
    return(str_c("(", new_rule, ")+"))
  } else if (rule_idx == 11) {
    return(rule_for_11())
  } else {
    return(new_rule)
  }
}

n_times_regex <- function(n) {
  str_c("{", n, ",", n, "}")
}

rule_for_11 <- function() {
  rule <- rules[12]
  parts <- str_split(rule, " ") %>% .[[1]] %>% as.integer()
  regex_subparts <- parts %>% 
    map_chr(m_rule_regex) %>% 
    map(~str_c("(", .x, ")"))
  # 8 is by trial and error
  map_chr(1:8, ~str_c(regex_subparts[1], n_times_regex(.x), regex_subparts[2], n_times_regex(.x))) %>%
    map(~str_c("(", .x, ")")) %>%
    str_c(collapse = "|") 
}

m_rule_regex <- memoise::memoise(rule_regex)

full_regex <- m_rule_regex(0)

input[2] %>% str_split('\n') %>% .[[1]] %>%
  map_lgl(~str_detect(., full_regex)) %>%
  sum()
  