library(tidyverse)
library(zeallot)

beacon_coords <- tibble(coords = read_file("solutions_2021/input19_sample2.txt")) %>% 
  separate_rows(coords, sep = "--- scanner \\d+ ---\n") %>% 
  filter(coords != "") %>% 
  mutate(scanner_id = 1:n()) %>% 
  separate_rows(coords, sep = "\n") %>% 
  filter(coords != "") %>% 
  separate(coords, c('orig_x', 'orig_y', 'orig_z'), sep = ",", convert = TRUE) %>% 
  nest(data = c(orig_x, orig_y, orig_z)) %>% 
  deframe()

# helpers --------------------------------------------------------

# half of these are actually mirror images, will be sorted out later
rotations <- function(x, y, z) {
  facing_directions <- cross3(c(-1, 1), c(-1, 1), c(-1, 1)) %>% 
    map(unlist)
  coord_variations <- combinat::permn(c(x, y, z))
  
  map(coord_variations, function(coords) {
    map(facing_directions, function(direction) {
      set_names(direction * coords, c("x", "y", "z"))
    })
  }) %>% 
    flatten() %>% 
    set_names(1:48)
}

scanner_with_all_rotations <- function(beacon_coords) {
  beacon_coords %>% 
    rowwise() %>% 
    mutate(rotation = list(rotations(orig_x, orig_y, orig_z))) %>% 
    unnest_longer(rotation) %>% 
    unnest_wider(rotation) %>% 
    select(rotation_id, x, y, z)
}

distances_for_scanner <- function(all_rotations_of_scanner) {
  all_rotations_of_scanner %>% 
    inner_join(all_rotations_of_scanner, by = 'rotation_id', suffix = c('', '2')) %>% 
    filter(x != x2  | y != y2 | y != y2) %>% 
    mutate(dx = x - x2, dy = y - y2, dz = z - z2) %>% 
    select(rotation_id, x, y, z, dx, dy, dz) %>% 
    nest(data = c(x, y, z, dx, dy, dz))
}

points_in_common_distance_matrix <- function(distances, known_beacon_distances) {
  distances %>%
    inner_join(known_beacon_distances, by = c('dx', 'dy', 'dz')) %>%
    select(known_x, known_y, known_z, x, y, z) %>%
    distinct()
}

get_scanner_position <- function(current_distances, correct_rotation, known_beacon_distances) {
  current_distances %>% 
    inner_join(correct_rotation, by = "rotation_id") %>% 
    pluck('data', 1) %>% 
    points_in_common_distance_matrix(known_beacon_distances) %>% 
    mutate(sx = x - known_x, sy = y - known_y, sz = z - known_z) %>% 
    select(sx, sy, sz) %>% 
    distinct()
}

# iterative scanner position matching  ----------------------------

scanner_positions <- tibble(sx = 0, sy = 0, sz = 0)

known_beacons <- select(beacon_coords[[1]], known_x = orig_x, known_y = orig_y, known_z = orig_z)

scanners_to_identify <- setdiff(names(beacon_coords), 1)

while(length(scanners_to_identify) > 0) {
  new_scanner_id <- scanners_to_identify[1]
  print(paste0("left to identify: ", length(scanners_to_identify), ", currently checking: ", new_scanner_id, "."))
  
  current_distances <- beacon_coords[[new_scanner_id]] %>% 
    scanner_with_all_rotations() %>% 
    distances_for_scanner()
  
  known_beacon_distances <- known_beacons %>% 
    inner_join(known_beacons, by = character(0), suffix = c('', '2')) %>% 
    filter(known_x != known_x2  | known_y != known_y2 | known_z != known_z2) %>% 
    mutate(dx = known_x - known_x2, dy = known_y - known_y2, dz = known_z - known_z2) %>% 
    select(known_x, known_y, known_z, dx, dy, dz)
  
  rotations_with_enough_common_pairwise_distances <- current_distances %>% 
    rowwise() %>% 
    mutate(common_distances = nrow(inner_join(data, known_beacon_distances, by = c('dx', 'dy', 'dz')))) %>% 
    filter(common_distances >= 12 * 11)
  
  if (nrow(rotations_with_enough_common_pairwise_distances) > 0) {
    
    correct_rotation <- rotations_with_enough_common_pairwise_distances %>%
      # with the mirror image there would be more distinct points as distances would be the same, but points would not align correctly
      filter(nrow(points_in_common_distance_matrix(data, known_beacon_distances)) == ceiling(sqrt(common_distances))) %>%
      select(rotation_id)
    
    scanner_position <- get_scanner_position(current_distances, correct_rotation, known_beacon_distances)
    
    new_known_beacons <- current_distances %>% 
      inner_join(correct_rotation, by = "rotation_id") %>% 
      pluck('data', 1) %>% 
      mutate(
        known_x = x - scanner_position$sx,
        known_y = y - scanner_position$sy,
        known_z = z - scanner_position$sz
      ) %>% 
      select(known_x, known_y, known_z)
    
    # record new knowledge
    scanner_positions <- rbind(scanner_positions, scanner_position)
    known_beacons <- rbind(known_beacons, new_known_beacons) %>% 
      distinct()
    scanners_to_identify <- setdiff(scanners_to_identify, new_scanner_id)
  } else {
    scanners_to_identify <- c(scanners_to_identify[2:length(scanners_to_identify)], scanners_to_identify[1])
  }
}

# part 1

nrow(known_beacons)

# part 2

scanner_positions %>% 
  inner_join(scanner_positions, by = character(0), suffix = c('1', '2')) %>% 
  mutate(manhattan = abs(sx1 - sx2) + abs(sy1 - sy2) + abs(sz1 - sz2)) %>% 
  pull('manhattan') %>% 
  max()

