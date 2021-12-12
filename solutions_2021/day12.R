library(tidyverse)

input <- tibble(x = read_lines("solutions_2021/input12.txt")) %>% 
  separate(x, c('from', 'to'))

caves <- unique(c(input$from, input$to))

small_caves <- caves[tolower(caves) == caves]

edges <- map(caves, ~{
  unique(c(
    filter(input, from == .) %>% pull(to),
    filter(input, to == .) %>% pull(from)
  ))
}) %>% 
  set_names(caves)

routes <- map(caves, ~tibble(to = character(0), n = integer(0), contains = list())) %>% set_names(caves)
new_routes <- routes
new_routes[["start"]] <- tibble(to = 'start', n = 1, contains = list(c("start")))

N_NEW_ROUTE <- map_dfr(new_routes, ~.) %>% pull(n) %>% sum()
routes <- map2(routes, new_routes, ~{
  bind_rows(.x, .y) %>% 
    group_by(to, contains) %>% 
    summarize(n = sum(n), .groups = "drop")
})

can_be_visited_p1 <- function(EDGE_END, contains) {
  !EDGE_END %in% small_caves || !EDGE_END %in% contains
}

can_be_visited_p2 <- function(EDGE_END, contains) {
  if (!EDGE_END %in% small_caves) return(TRUE)
  visited_small_caves <- contains[contains %in% small_caves]
  if (EDGE_END %in% c('start', 'end') && EDGE_END %in% visited_small_caves) return(FALSE)
  if (n_distinct(visited_small_caves) < length(visited_small_caves) && EDGE_END %in% visited_small_caves) return(FALSE)
  return(TRUE)
}

while(N_NEW_ROUTE > 0) {
  new_routes <- imap(edges, function(froms, EDGE_END) {
    froms %>% 
      map_dfr(function(from) {
        routes[[from]] %>% 
          rowwise() %>% 
          filter(can_be_visited_p2(EDGE_END, contains)) %>% 
          ungroup()
      }) %>% 
      mutate(to = EDGE_END) %>% 
      group_by(to, contains) %>% 
      summarize(n = sum(n), .groups = "drop") %>% 
      mutate(contains = map(contains, ~sort(unlist(c(EDGE_END, .))))) %>% 
      anti_join(routes[[EDGE_END]], by = c('to', 'contains'))
  })
  N_NEW_ROUTE <- map_dfr(new_routes, ~.) %>% pull(n) %>% sum()
  routes <- map2(routes, new_routes, ~{
    bind_rows(.x, .y) %>% 
      group_by(to, contains) %>% 
      summarize(n = sum(n), .groups = "drop")
  })
}

routes[['end']] %>% pull(n) %>% sum()