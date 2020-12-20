library(tidyverse)

input <- read_file("solutions/day20_input.txt")

tiles <- input %>% str_split("\n\n") %>% .[[1]] %>%
  tibble(x = .) %>%
  extract(x, into = c('id', 'image'), regex = "Tile (\\d+):\n([.#\n]+)", convert = TRUE) %>%
  separate_rows(image, sep = "\n") %>%
  group_by(id) %>%
  mutate(row = 1:n()) %>%
  ungroup() %>%
  separate_rows(image, sep = "") %>%
  rename(pixel = 'image') %>%
  filter(pixel != "") %>%
  group_by(id, row) %>%
  mutate(col = 1:n()) %>%
  ungroup()

pixels_to_int <- function(pixels) {
  bits <- pixels %>%
    str_replace_all("[#]", "1") %>%
    str_replace_all("[.]", "0") %>%
    str_split("") %>% .[[1]] %>%
    as.integer()
  
  return(c("forward" = sum(bits * 2^(0:9)), "backward" = sum(bits * 2^(9:0))))
}

tile_edges <- tiles %>%
  group_by(id) %>%
  arrange(id, row, col) %>%
  summarize(
    top_row = str_c(pixel[row == 1], collapse = ""),
    bottom_row = str_c(pixel[row == 10], collapse = ""),
    right_col = str_c(pixel[col == 10], collapse = ""),
    left_col = str_c(pixel[col == 1], collapse = "")
  ) %>%
  pivot_longer(
    c(top_row, bottom_row, right_col, left_col),
    values_to = "edge", names_to = "edge_name"
  ) %>%
  rowwise() %>%
  mutate(
    edge_as_int = list(pixels_to_int(edge))
  ) %>%
  ungroup()

tile_edges %>%
  unnest_auto(edge_as_int) %>%
  pivot_longer(c(forward, backward), names_to = "direction", values_to = "edge_as_int") %>%
  add_count(edge_as_int) %>%
  filter(n == 1) %>%
  count(id) %>%
  filter(n == 4) %>%
  pull(id) %>%
  as.numeric() %>%
  reduce(`*`) %>%
  sprintf("%.0f", .)
