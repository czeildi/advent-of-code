library(tidyverse)

input <- 347991

# part 1
size <- floor(sqrt(input))
left <- input - size ^ 2
left - (size + 1)

# part 2

directions <- cross2(-1:1, -1:1) %>% map(unlist)

point <- function(coords) paste(coords, collapse = ";")

next_coords <- function(coords) {
  x = coords[1]
  y = coords[2]
  layer = max(abs(x), abs(y))
  if (x <= layer && y == -layer) return (c(x + 1, y))
  if (x == layer && y < layer) return (c(x, y + 1))
  if (x > -layer && y == layer) return (c(x - 1, y))
  if (x == -layer && y > -layer) return (c(x, y - 1))
}

coords <- c(0, 0)
seen <- integer(0)
value_to_write <- 1
seen[point(coords)] = value_to_write

while(value_to_write <= input) {
  coords <- next_coords(coords)
  neighbors <- map_chr(directions, ~ point(coords + .))
  value_to_write <- sum(seen[neighbors], na.rm = TRUE)
  seen[point(coords)] = value_to_write
}

value_to_write
