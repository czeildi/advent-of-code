library(tidyverse)
library(zeallot)

input <- read_file("solutions_2020/day20_input.txt")

tiles <- input %>% str_split("\n\n") %>% .[[1]] %>%
  tibble(x = .) %>%
  extract(x, into = c('id', 'image'), regex = "Tile (\\d+):\n([.#\n]+)", convert = TRUE) %>%
  separate_rows(image, sep = "\n") %>%
  group_by(id) %>%
  mutate(row = 1:n()) %>%
  separate_rows(image, sep = "") %>%
  rename(pixel = 'image') %>%
  filter(pixel != "") %>%
  group_by(id, row) %>%
  mutate(col = 1:n()) %>%
  ungroup()

tile_width <- max(tiles$row)

# part 1 ------------------------------------------------------------------

tile_edges <- tiles %>%
  group_by(id) %>%
  arrange(id, row, col) %>%
  summarize(
    edge_0 = str_c(pixel[row == 1], collapse = ""),
    edge_1 = str_c(pixel[col == tile_width], collapse = ""),
    edge_2 = str_c(pixel[row == tile_width], collapse = ""),
    edge_3 = str_c(pixel[col == 1], collapse = "")
  ) %>%
  pivot_longer(
    matches('edge_[0-3]'), names_prefix = "edge_",
    values_to = "edge", names_to = "edge_id"
  ) %>%
  mutate(edge_id = as.integer(edge_id)) %>%
  mutate(edge_value = pmin(edge, stringi::stri_reverse(edge))) %>%
  add_count(edge_value, name = "edge_freq")

corner_ids <- tile_edges %>%
  filter(edge_freq == 1) %>%
  count(id, name = "n_edge_without_pair") %>%
  filter(n_edge_without_pair == 2) %>%
  pull(id)

corner_ids %>%
  as.numeric() %>%
  reduce(`*`) %>%
  sprintf("%.0f", .)


# part 2 ------------------------------------------------------------------

image_width_in_tiles <- sqrt(n_distinct(tiles$id))
monster <- read_lines("solutions_2020/day20_monster.txt") %>%
  str_replace_all(' ', '.')
n_monster_hashmark <- str_count(monster, "#") %>% sum()

next_matched_edge <- function(tile_coords, tile_edges, tile_x, tile_y, direction) {
  if (direction == 'left') {
    params <- list(x_diff = -1, y_diff = 0, edge_name = "right_edge_id")
  } else {
    params <- list(x_diff = 0, y_diff = -1, edge_name = "bottom_edge_id")
  }
  tile_to_match <- filter(tile_coords, x == tile_x +params$x_diff & y == tile_y + params$y_diff)
  edge_to_match <- tile_edges %>%
    filter(id == tile_to_match$id & edge_id == tile_to_match[[params$edge_name]]) %>%
    select(edge_value)
  tile_edges %>%
    inner_join(edge_to_match, by = "edge_value", copy = TRUE) %>%
    anti_join(tile_coords, by = "id")
}

next_tile_in_top_row <- function(tile_coords, tile_edges, tile_x, tile_y) {
  matched_edge <- next_matched_edge(tile_coords, tile_edges, tile_x, tile_y, "left")
  
  right_edge_id <- (matched_edge$edge_id + 2) %% 4
  top_edge_id <- tile_edges %>%
    filter(id == matched_edge$id & edge_id != right_edge_id & edge_freq == 1) %>%
    pull(edge_id)
  bottom_edge_id <- (top_edge_id + 2) %% 4
  
  list(id = matched_edge$id, bottom_edge_id = bottom_edge_id, right_edge_id = right_edge_id)
}

next_tile_in_left_col <- function(tile_coords, tile_edges, tile_x, tile_y) {
  matched_edge <- next_matched_edge(tile_coords, tile_edges, tile_x, tile_y, "top")
  
  bottom_edge_id <- (matched_edge$edge_id + 2) %% 4
  left_edge_id <- tile_edges %>%
    filter(id == matched_edge$id & edge_id != bottom_edge_id & edge_freq == 1) %>%
    pull(edge_id)
  right_edge_id <- (left_edge_id + 2) %% 4
  
  list(id = matched_edge$id, bottom_edge_id = bottom_edge_id, right_edge_id = right_edge_id)
}

next_tile_general <- function(tile_coords, tile_edges, tile_x, tile_y) {
  left_matched_edge <- next_matched_edge(tile_coords, tile_edges, tile_x, tile_y, "left")
  top_matched_edge <- next_matched_edge(tile_coords, tile_edges, tile_x, tile_y, "top")
  
  right_edge_id <- (left_matched_edge$edge_id + 2) %% 4
  bottom_edge_id <- (top_matched_edge$edge_id + 2) %% 4
  
  list(id = left_matched_edge$id, bottom_edge_id = bottom_edge_id, right_edge_id = right_edge_id)
}

arrange_tiles <- function(tiles_with_positions) {
  tiles_with_positions %>%
    mutate(
      new_row = case_when(
        rotation == 0 ~ tile_width + 1L - row,
        rotation == 1 ~ col,
        rotation == 2 ~ row,
        rotation == 3 ~ tile_width + 1L - col
      ),
      new_col = case_when(
        rotation == 0 ~ tile_width + 1L - col,
        rotation == 1 ~ tile_width + 1L - row,
        rotation == 2 ~ col,
        rotation == 3 ~ row
      )
    ) %>%
    mutate(row = new_row, col = new_col) %>%
    mutate(
      col = if_else(is_flipped == TRUE, tile_width + 1L - col, col)
    )
}

assemble_image_rows <- function(pixels) {
  pixels %>%
    group_by(x, col) %>%
    arrange(y, -row) %>%
    summarize(image_row = str_c(pixel, collapse = "")) %>%
    arrange(x, -col) %>%
    select(image_row)
}

pattern_starts <- function(text, pattern) {
  map_dbl(seq_len(nchar(text)), ~str_locate(str_sub(text, .), pattern)[, 1] + . - 1) %>% 
    unique() %>%
    .[!is.na(.)]
}

monster_matches <- function(whole_image, monster) {
  whole_image %>%
    rowwise() %>%
    mutate(
      matches_monster_1 = list(pattern_starts(image_row, monster[1])),
      matches_monster_2 = list(pattern_starts(image_row, monster[2])),
      matches_monster_3 = list(pattern_starts(image_row, monster[3]))
    ) %>%
    ungroup() %>%
    mutate(
      previous_row_matches_monster_1 = lag(matches_monster_1),
      next_row_matches_monster_3 = lead(matches_monster_3)
    ) %>%
    rowwise() %>%
    mutate(matches_middle = list(reduce(
      list(previous_row_matches_monster_1, matches_monster_2, next_row_matches_monster_3),
      intersect
    )))
}

determine_tile_coords <- function(selected_top_left_id, initial_tile_right_edge_id, initial_tile_bottom_edge_id) {
  tile_coords <- tibble(
    id = selected_top_left_id,
    x = 0,
    y = 0,
    bottom_edge_id = initial_tile_bottom_edge_id,
    right_edge_id = initial_tile_right_edge_id
  )
  
  for (tile_id in 1:(image_width_in_tiles ^ 2 - 1)) {
    tile_x <- tile_id %% image_width_in_tiles
    tile_y <- (tile_id - tile_x) / image_width_in_tiles
    
    if (tile_x != 0 && tile_y == 0) {
      next_tile <- next_tile_in_top_row(tile_coords, tile_edges, tile_x, tile_y)
    } else if (tile_x == 0) {
      next_tile <- next_tile_in_left_col(tile_coords, tile_edges, tile_x, tile_y)
    } else {
      next_tile <- next_tile_general(tile_coords, tile_edges, tile_x, tile_y)
    }
    
    tile_coords <- rbind(
      tile_coords, 
      tibble(
        id = next_tile$id,
        x = tile_x,
        y = tile_y,
        bottom_edge_id = next_tile$bottom_edge_id,
        right_edge_id = next_tile$right_edge_id
      )
    )  
  }
  tile_coords
}

not_monster_tiles_with_params <- function(selected_top_left_id, initial_tile_right_edge_id, initial_tile_bottom_edge_id) {
  tile_coords <- determine_tile_coords(
    selected_top_left_id, initial_tile_right_edge_id, initial_tile_bottom_edge_id
  )
  
  tile_positions <- tile_coords %>%
    mutate(
      rotation = (bottom_edge_id - 2 + 4) %% 4,
      is_flipped = bottom_edge_id != (right_edge_id + 1) %% 4
    ) %>% 
    select(id, x, y, rotation, is_flipped)
  
  tiles_with_positions <- tiles %>%
    filter(row != 1 & row != tile_width & col != 1 & col != tile_width) %>%
    inner_join(tile_positions, by = "id")
  
  arranged_tiles <- arrange_tiles(tiles_with_positions)
  
  whole_image <- arranged_tiles %>%
    assemble_image_rows() %>%
    ungroup() %>%
    mutate(row_idx = 1:n()) %>%
    select(row_idx, image_row)
  
  n_total_hashmark <- whole_image %>%
    mutate('n_hashmark' = str_count(image_row, "#")) %>%
    pull(n_hashmark) %>%
    sum()
  
  total_monster_count <- whole_image %>%
    monster_matches(monster) %>%
    mutate(times_matches_monster = length(matches_middle)) %>%
    {sum(.$times_matches_monster)}
  
  n_total_hashmark - n_monster_hashmark * total_monster_count
}

map(corner_ids, ~{
  selected_top_left_id <- .x
  
  right_and_bottom_edge_id <- tile_edges %>% 
    filter(id == selected_top_left_id) %>%
    select(edge_id, edge_freq) %>%
    filter(edge_freq == 2) %>%
    pull(edge_id)
  
  c(
    not_monster_tiles_with_params(selected_top_left_id, min(right_and_bottom_edge_id), max(right_and_bottom_edge_id)),
    not_monster_tiles_with_params(selected_top_left_id, max(right_and_bottom_edge_id), min(right_and_bottom_edge_id))
  )
}) %>%
  unlist() %>%
  min()
