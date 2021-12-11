library(tidyverse)
library(glue)

before_step <- tibble(value = read_lines("solutions_2021/input11.txt")) %>% 
  mutate(y = 1:n()) %>% 
  separate_rows(value, sep = "", convert = TRUE) %>% 
  filter(!is.na(value)) %>% 
  group_by(y) %>% 
  mutate(x = 1:n()) %>% 
  ungroup()

directions <- tribble(
  ~direction, ~dx, ~dy,
  "r", 1, 0,
  "l", -1, 0,
  "b", 0, 1,
  "t", 0, -1,
  "br", 1, 1,
  "bl", -1, 1,
  "tl", -1, -1,
  "tr", 1, -1,
)

N_TOTAL_FLASH <- 0

STEP_IDX <- 1

# steps start

# ggplot(before_step, aes(x, y = 10 - y, fill = value == 0)) + 
#   geom_tile() + 
#   geom_label(aes(label = value))

N_FLASH_WITHIN_STEP <- 0
while (N_FLASH_WITHIN_STEP != 100) { # STEP_IDX <= 100 for part 1
  N_FLASH_WITHIN_STEP <- 0
  
  after_step_part <- before_step %>% 
    mutate(value = value + 1) %>% 
    mutate(is_flashed = value == 10)
  
  while (sum(after_step_part$is_flashed) > 0) {
    N_FLASH_WITHIN_STEP <- N_FLASH_WITHIN_STEP + sum(after_step_part$is_flashed)
    
    after_step_part <- after_step_part %>% 
      inner_join(directions, by = character()) %>% 
      mutate(nb_x = x + dx, nb_y = y+ dy) %>% 
      select(-c(direction, dx, dy)) %>% 
      inner_join(after_step_part, by = c("nb_x" = "x", "nb_y" = "y")) %>% 
      rename(value = value.x, nb_value = value.y, is_flashed = is_flashed.x, nb_is_flashed = is_flashed.y) %>% 
      group_by(x, y, value, is_flashed) %>% 
      summarize(n_nb_flashed = sum(nb_is_flashed), .groups = "drop") %>% 
      ungroup() %>% 
      mutate(
        is_flashed = value < 10 & (value + n_nb_flashed) >= 10,
        value = value + n_nb_flashed
      )
    
    after_step_part <- after_step_part %>% 
      select(x, y, value, is_flashed)
  }
  
  # step end
  
  before_step <- after_step_part %>% 
    mutate(value = if_else(value >= 10, 0, value))
  N_TOTAL_FLASH <- N_TOTAL_FLASH + N_FLASH_WITHIN_STEP
  print(STEP_IDX)
  print(N_FLASH_WITHIN_STEP)
  STEP_IDX <- STEP_IDX + 1
}

N_TOTAL_FLASH
