library(tidyverse)

input <- tibble(value = read_lines("solutions_2021/input25.txt")) %>% 
  separate_rows(value, sep = "\n") %>% 
  mutate(y = 1:n()) %>% 
  separate_rows(value, sep = "") %>% 
  filter(value != "") %>% 
  group_by(y) %>% 
  mutate(x = 1:n()) %>% 
  ungroup()

width <- max(input$x)
height <- max(input$y)

grid <- input %>% 
  filter(value != ".")

east_step <- function(grid) {
  grid %>% 
    mutate(rx = if_else(x == width, 1L, x + 1L)) %>% 
    left_join(grid, by = c("rx" = "x", "y" = "y"), suffix = c("", "_r")) %>% 
    mutate(x = if_else(value == ">" & is.na(value_r), rx, x)) %>% 
    select(value, x, y)
}

south_step <- function(grid) {
  grid %>% 
    mutate(by = if_else(y == height, 1L, y + 1L)) %>% 
    left_join(grid, by = c("by" = "y", "x" = "x"), suffix = c("", "_b")) %>% 
    mutate(y = if_else(value == "v" & is.na(value_b), by, y)) %>% 
    select(value, x, y)
}

step_idx <- 1
new_grid <- south_step(east_step(grid))

while(all.equal(grid, new_grid) != TRUE) {
  print(paste0(Sys.time(), "doing step: ", step_idx))
  grid <- new_grid
  new_grid <- south_step(east_step(grid))
  step_idx <- step_idx + 1
}

step_idx