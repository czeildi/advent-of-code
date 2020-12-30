library(tidyverse)

input <- tibble(x = read_lines("solutions_2020/day21_input.txt")) %>%
  mutate(food_id = 1:n()) %>%
  separate(x, into = c("ingredients", "allergenes"), sep = "\\(") %>%
  mutate(allergenes = str_replace_all(allergenes, "contains ", "")) %>%
  mutate(allergenes = str_replace_all(allergenes, "\\)", "")) %>%
  rowwise() %>%
  mutate(
    ingredients_l = list(str_split(ingredients, " ") %>% .[[1]] %>% .[. != '']),
    allergenes_l = list(str_split(allergenes, ", ") %>% .[[1]] %>% .[. != ''])
  ) %>%
  ungroup()

n_allergenes <- n_distinct(unlist(input$allergenes_l))

options_for_allergenes <- input %>%
  separate_rows(allergenes) %>%
  group_by(allergenes) %>%
  summarize(
    ingredient = list(reduce(ingredients_l, intersect))
  ) %>%
  unnest(ingredient)

matched_allergenes <- tibble(ingredient = character(0), allergene = character(0))

while(nrow(matched_allergenes) < n_allergenes) {
  newly_matched_allergenes <- options_for_allergenes %>%
    anti_join(select(matched_allergenes, ingredient), by = "ingredient") %>%
    group_by(allergenes) %>%
    mutate(n_option = n()) %>%
    filter(n_option == 1) %>%
    select(ingredient, allergene = allergenes)
  matched_allergenes <<- rbind(matched_allergenes, newly_matched_allergenes)
}

input %>%
  unnest(ingredients_l) %>%
  anti_join(matched_allergenes, by = c("ingredients_l" = "ingredient"))

matched_allergenes %>%
  arrange(allergene) %>%
  pull(ingredient) %>%
  str_c(collapse = ",")
