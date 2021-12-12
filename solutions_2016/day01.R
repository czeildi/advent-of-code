library(tidyverse)

steps <- readLines("solutions_2016/input01.txt") %>% str_split(", ") %>% pluck(1)

directions = list(
  'n' = c(0, 1),
  'e' = c(1, 0),
  's' = c(0, -1),
  'w' = c(-1, 0)
)

turns <- list(
  'n' = list('R' = 'e', 'L' = 'w'),
  'e' = list('R' = 's', 'L' = 'n'),
  's' = list('R' = 'w', 'L' = 'e'),
  'w' = list('R' = 'n', 'L' = 's')
)


is_in <- function(vector, list_of_vectors) {
  length(keep(list_of_vectors, ~ all(. == vector))) > 0
}

p2_result <- NULL
visited_locations <- list(c(0, 0))

final_position <- reduce(steps, function(prev, current) {
  pieces <- str_split_fixed(current, "", 2)
  turn <- pieces[1, 1]
  step_n = as.numeric(pieces[1, 2])
  
  facing = turns[[prev$facing]][[turn]]
  position = prev$position + directions[[facing]] * step_n
  
  walk2(prev$position[1]:position[1], prev$position[2]:position[2], ~{
    visited = c(.x, .y)
    if (all.equal(prev$position, visited) != TRUE) {
      if (is_in(visited, visited_locations) && is.null(p2_result)) {
       p2_result <<- sum(abs(visited))
      }
      visited_locations <<- c(visited_locations, list(visited))
    }
  })
  
  list(facing = facing, position = position)
}, .init = list(facing = 'n', position = c(0, 0)))

sum(abs(final_position$position))

p2_result