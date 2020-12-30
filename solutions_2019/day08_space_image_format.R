library(tidyverse)

image_layers <- read_file("./solutions_2019/day08_input.txt") %>%
  str_extract_all("\\d{150,150}") %>%
  .[[1]] %>%
  {tibble(pixels = .)}

# part 1 ----------------------------------------------------------------------
image_layers %>%
  rowwise() %>%
  mutate(
    n_0 = str_count(pixels, "0"),
    n_1_m_n_2 = str_count(pixels, "1") * str_count(pixels, "2")
  ) %>%
  arrange(n_0) %>%
  pull(n_1_m_n_2) %>%
  head(1)

# part 2 -----------------------------------------------------------------------

visible_pixels <- image_layers %>%
  mutate(layer_idx = 1:n()) %>%
  separate_rows(pixels, sep = "") %>%
  filter(pixels != "") %>%
  group_by(layer_idx) %>%
  mutate(pixel_position = 1:n()) %>%
  filter(pixels != "2") %>%
  group_by(pixel_position) %>%
  arrange(layer_idx) %>%
  summarize(visible_pixel = first(pixels))

visible_pixels %>%
  mutate(row = (pixel_position - 1) %/% 25, col = (pixel_position - 1) %% 25) %>%
  group_by(row) %>%
  arrange(col) %>%
  summarize(pixels = str_c(visible_pixel, collapse = ""))
