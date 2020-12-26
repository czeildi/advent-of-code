library(tidyverse)

trees <- read_lines('solutions_2020/day03_input.txt')
tree_map <- lapply(trees, function(row) {
  str_split(row, '')[[1]] %>%
    map_lgl(~.=='#')
})

map_width <- str_length(trees[1])

# part 1 ------------------------------------------------------------------

imap_lgl(tree_map, ~{
  index_within_row <- (1 + 3 * (.y - 1)) %% map_width
  if (index_within_row == 0) index_within_row <- map_width
  .x[index_within_row]
}) %>% 
  sum()

# part 2 ------------------------------------------------------------------

slopes <- tribble(
  ~right, ~down,
  1,1,
  3,1,
  5,1,
  7,1,
  1,2
)

num_trees_with_slope <- function(tree_map, right, down) {
  map_width <- length(tree_map[[1]])
  imap_lgl(tree_map, ~{
    if (((.y - 1) %% down) != 0) {
      return(FALSE)
    }
    step_index <- floor((.y - 1) / down)
    index_within_row <- (1 + right * step_index) %% map_width
    if (index_within_row == 0) index_within_row <- map_width
    .x[index_within_row]
  }) %>% 
    sum()
}

pmap_int(slopes, function(right, down) num_trees_with_slope(tree_map, right, down)) %>% 
  reduce(`*`, .init = 1)
