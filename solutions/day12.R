library(tidyverse)

input <- tibble(x = read_lines("solutions/day12_input.txt")) %>%
  separate(x, c('direction', 'amount'), sep = 1, extra = 'merge') %>%
  mutate(amount = case_when(
    direction == 'L' ~ as.numeric(amount) / 90,
    direction == 'R' ~ as.numeric(amount) / 90,
    TRUE ~ as.numeric(amount)
  )) %>%
  mutate(row_index = seq_len(nrow(.)))

# would be a different task if turn degree is not always K * 90

input %>%
  filter(direction == 'L' | direction == 'R') %>%
  distinct(amount)


poles <- c('E', 'S', 'W', 'N')
possible_directions <- c('L', 'R', 'F', poles)

calculate_new_face_toward <- function(direction, amount, previous_face_toward) {
  if (!direction %in% c('L', 'R')) return (previous_face_toward)
  current_face_index <- which(poles == previous_face_toward)
  if (direction == 'L') new_face_index <- (current_face_index - amount) %% 4
  if (direction == 'R') new_face_index <- (current_face_index + amount) %% 4
  if (new_face_index == 0) new_face_index <- 4
  poles[new_face_index]
}

new_face_table <- crossing(direction = possible_directions, amount = 1:3, previous_face_toward = poles) %>%
  rowwise() %>%
  mutate(new_face_toward = calculate_new_face_toward(direction, amount, previous_face_toward))

command_to_move <- function(current_direction, current_amount, face_toward) {
  new_face <- new_face_table %>%
    filter(direction == current_direction & previous_face_toward == face_toward) %>%
    filter(amount == current_amount | (current_direction != 'L' & current_direction != 'R')) %>%
    pull(new_face_toward) %>%
    head(1)
  if (current_direction %in% c('L', 'R')) {
    return(list('face' = new_face, 'x_diff' = 0, 'y_diff' = 0))
  }
  if ((new_face == 'E' && current_direction == 'F') || current_direction == 'E') {
    return(list('face' = new_face, 'x_diff' = current_amount, 'y_diff' = 0))
  }
  if ((new_face == 'S' && current_direction == 'F') || current_direction == 'S') {
    return(list('face' = new_face, 'x_diff' = 0, 'y_diff' = current_amount))
  }
  if ((new_face == 'W' && current_direction == 'F') || current_direction == 'W') {
    return(list('face' = new_face, 'x_diff' = -current_amount, 'y_diff' = 0))
  }
  if ((new_face == 'N' && current_direction == 'F') || current_direction == 'N') {
    return(list('face' = new_face, 'x_diff' = 0, 'y_diff' = -current_amount))
  }
}

x <- 0
y <- 0
face <- 'E'

for (command_index in seq_len(nrow(input))) {
  command <- command_to_move(
    current_direction = input %>% filter(row_index == command_index) %>% pull(direction),
    current_amount = input %>% filter(row_index == command_index) %>% pull(amount),
    face_toward = face
  )
  x <<- x + command$x_diff
  y <<- y + command$y_diff
  face <<- command$face
}

abs(x) + abs(y)
