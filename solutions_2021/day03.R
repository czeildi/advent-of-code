library(tidyverse)

cucc <- tibble(x = read_lines("solutions_2021/day03_input.txt")) %>% 
  mutate(id = 1:n()) %>% 
  separate(x, into = paste0('x', 1:12), sep= 1:11, convert = TRUE)

# part 1 was overwritten accidentally and lost

# part 2

column_idx <- 1
cucc_1 <- cucc
cucc_0 <- cucc

common_1 <- function(values) {
  if_else(sum(values) >= length(values) / 2, 1, 0)
}

common_0 <- function(values) {
  if_else(sum(values) < length(values) / 2, 1, 0)
}

while(nrow(cucc_1) > 1) {
  col_name <- paste0('x', column_idx)
  cucc_1 <<- cucc_1[cucc_1[[col_name]] == common_1(cucc_1[[col_name]]),]
  column_idx <<- column_idx + 1
}
cucc_1

column_idx <- 1

while(nrow(cucc_0) > 1) {
  col_name <- paste0('x', column_idx)
  cucc_0 <<- cucc_0[cucc_0[[col_name]] == common_0(cucc_0[[col_name]]),]
  column_idx <<- column_idx + 1
}
cucc_0

# convert 2 numbers to decimal manually