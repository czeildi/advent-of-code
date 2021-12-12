library(tidyverse)

input <- tibble(x = read_lines("solutions_2021/input12_sample1.txt")) %>% 
  separate(x, c('from', 'to'))

caves <- unique(c(input$from, input$to))

edges <- map(caves, ~{
  unique(c(
    filter(input, from == .) %>% pull(to),
    filter(input, to == .) %>% pull(from)
  ))
}) %>% 
  set_names(caves)

small_caves <- caves[tolower(caves) == caves]

can_be_visited_p1 <- function(route_end, route_points) {
  !route_end %in% small_caves || !route_end %in% route_points
}

can_be_visited_p2 <- function(route_end, route_points) {
  if (!route_end %in% small_caves) return(TRUE)
  visited_small_caves <- route_points[route_points %in% small_caves]
  if (route_end %in% c('start', 'end') && route_end %in% visited_small_caves) return(FALSE)
  if (n_distinct(visited_small_caves) < length(visited_small_caves) && route_end %in% visited_small_caves) return(FALSE)
  return(TRUE)
}

routes <- map(caves, ~list()) %>% set_names(caves)
routes[["start"]] <- list(c('start'))

N_NEW_ROUTE <- 1

while(N_NEW_ROUTE > 0) {
  new_routes <- imap(edges, function(edge_starts, end_end) {
    map(edge_starts, function(from) {
        routes[[from]] %>% 
          keep(function(route) can_be_visited_p2(end_end, route)) %>% 
          map(function(route) c(end_end, route))
    }) %>% 
      flatten() %>% 
      setdiff(routes[[end_end]])
  })
  N_NEW_ROUTE <- sum(map_int(new_routes, length))
  routes <- map2(routes, new_routes, ~c(.x, .y))
}

length(routes[['end']])