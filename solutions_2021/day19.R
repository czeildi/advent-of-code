library(tidyverse)
library(zeallot)

beacon_coords <- tibble(coords = read_file("solutions_2021/input19_sample2.txt")) %>% 
  separate_rows(coords, sep = "--- scanner \\d+ ---\n") %>% 
  filter(coords != "") %>% 
  mutate(scanner_id = 1:n()) %>% 
  separate_rows(coords, sep = "\n") %>% 
  filter(coords != "") %>% 
  separate(coords, c('x', 'y', 'z'), sep = ",", convert = TRUE)

# helpers --------------------------------------------------------

# half of these are actually mirror images, will be sorted out later
rotations <- function(x, y, z) {
  rot <- list(
    c(1, 1, 1),
    c(1, 1, -1),
    c(1, -1, 1),
    c(-1, 1, 1),
    c(1, -1, -1),
    c(-1, 1, -1),
    c(-1, -1, 1),
    c(-1, -1, -1)
  )
  map(
    list(c(x, y, z), c(x, z, y), c(y, z, x), c(y, x, z), c(z, x, y), c(z, y, x)),
    function(mixed) {
      map(rot, ~ (. * mixed) %>% set_names(letters[1:3]))
    }
  ) %>% flatten() %>% 
    set_names(1:48)
}

scanner_with_all_rotations <- function(beacon_coords) {
  beacon_coords %>% 
    rowwise() %>% 
    mutate(coords = list(rotations(x, y, z))) %>% 
    unnest_longer(coords) %>% 
    unnest_wider(coords)
}

distances_for_scanner <- function(current_scanner) {
  current_scanner %>% 
    inner_join(current_scanner, by = 'coords_id', suffix = c('1', '2')) %>% 
    filter(x1 != x2 | y1 != y2 | z1 != z2) %>% 
    mutate(ad = a1 - a2, bd = b1 - b2, cd = c1 - c2) %>% 
    select(coords_id, a1, b1, c1, ad, bd, cd) %>% 
    nest(data = c(a1, b1, c1, ad, bd, cd))
}

count_of_points_in_common_distance_matrix <- function(distances, known_beacon_distances) {
  distances %>%
    inner_join(known_beacon_distances, by = c('ad', 'bd', 'cd')) %>%
    select(x_known, y_known, z_known, a1, b1, c1) %>%
    distinct() %>%
    nrow()
}

get_scanner_position <- function(current_distances, correct_rotation, known_beacon_distances) {
  current_distances %>% 
    inner_join(correct_rotation, by = "coords_id") %>% 
    pluck('data', 1) %>% 
    inner_join(known_beacon_distances, by = c('ad', 'bd', 'cd')) %>% 
    select(x_known, y_known, z_known, a1, b1, c1) %>% 
    distinct() %>% 
    mutate(sx = a1 - x_known, sy = b1 - y_known, sz = c1 - z_known) %>% 
    select(sx, sy, sz) %>% 
    distinct()
}

# iterative scanner position matching  ----------------------------

scanner_positions <- tibble(sx = 0, sy = 0, sz = 0)

known_beacons <- filter(beacon_coords, scanner_id == 1) %>% 
  select(-scanner_id) %>% 
  rename(a = x, b = y, c = z)

scanners_to_identify <- setdiff(unique(beacon_coords$scanner_id), 1)

while(length(scanners_to_identify) > 0) {
  new_scanner_id <- scanners_to_identify[1]
  print(paste0("left to identify: ", length(scanners_to_identify), ", currently checking: ", new_scanner_id, "."))
  
  beacon_coords_to_identify <- beacon_coords %>% 
    filter(scanner_id == new_scanner_id) %>% 
    scanner_with_all_rotations()
  current_distances <- distances_for_scanner(beacon_coords_to_identify)
  
  known_beacon_distances <- known_beacons %>% 
    inner_join(known_beacons, by = character(0), suffix = c('1', '2')) %>% 
    filter(a1 != a2 | b1 != b2 | c1 != c2) %>% 
    mutate(ad = a1 - a2, bd = b1 - b2, cd = c1 - c2) %>% 
    select(ad, bd, cd, x_known = a1, y_known = b1, z_known = c1)
  
  rotations_with_enough_common_pairwise_distances <- current_distances %>% 
    rowwise() %>% 
    mutate(common_distances = nrow(inner_join(data, known_beacon_distances, by = c('ad', 'bd', 'cd')))) %>% 
    filter(common_distances >= 12 * 11)
  
  if (nrow(rotations_with_enough_common_pairwise_distances) > 0) {
    
    correct_rotation <- rotations_with_enough_common_pairwise_distances %>%
      # with the mirror image there would be more distinct points as distances would be the same, but points would not align correctly
      filter(count_of_points_in_common_distance_matrix(data, known_beacon_distances) == ceiling(sqrt(common_distances))) %>%
      select(coords_id)
    
    scanner_position <- get_scanner_position(current_distances, correct_rotation, known_beacon_distances)
    scanner_positions <- rbind(scanner_positions, scanner_position)
    
    new_known_beacons <- beacon_coords_to_identify %>% 
      inner_join(correct_rotation, by = "coords_id") %>% 
      mutate(
        a = a - scanner_position$sx,
        b = b - scanner_position$sy,
        c = c - scanner_position$sz
      ) %>% 
      select(a, b, c)
    
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
  arrange(desc(manhattan)) %>% 
  head(1) %>% 
  pluck('manhattan')