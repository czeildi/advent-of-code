library(tidyverse)
library(glue)
library(memoise)
library(tidygraph)
options(scipen = 999)

year <- "2022"
day <- "16"
input_file <- glue("solutions_{year}/day{day}_input_sample.txt")

input <- tibble(x = read_lines(input_file))

parsed <- input |>
  extract(
    x, c("name", "flow", "neighbor"),
    regex = "Valve ([A-Z]+) has flow rate=([0-9]+); tunnels? leads? to valves? (.*)",
    convert = TRUE
  ) |>
  mutate(idx = row_number())

valves <- select(parsed, idx, name, flow)
tunnels <- select(parsed, idx, neighbor) |>
  separate_rows(neighbor) |>
  inner_join(valves, by = c("neighbor" = "name"), suffix = c(".from", ".to")) |>
  select(from = idx.from, to = idx.to)

important_valves <- filter(valves, flow > 0)

g <- tbl_graph(
  nodes = select(valves, node_id = idx, name),
  edges = tunnels
)

pairwise_distances <- do.call(rbind, lapply(important_valves$idx, function(to_id) {
  g |>
    activate(nodes) |>
    mutate(dist = node_distance_to(to_id)) |>
    activate(nodes) |>
    as_tibble() |>
    mutate(to_id = to_id)
})) |>
  select(from_id = node_id, from_name = name, dist, to_id) |>
  inner_join(select(valves, idx, to_name = name), by = c("to_id" = "idx"))

distances <- pairwise_distances |>
  group_by(from_name) |>
  summarise(distances = list(deframe(tibble(to_name, dist)))) |>
  deframe()

valve_flows <- select(important_valves, name, flow) |> deframe()

# part 1
get_max_flow <- function(
  current_position,
  closed_valves,
  remaining_time
) {
  if (remaining_time <= 0) return(0)
  if (length(closed_valves) == 0) return(0)

  distances_to_closed_valves <- distances[[current_position]][closed_valves]

  sapply(names(distances_to_closed_valves), function(next_valve_to_open) {
    remaining_time_after_opening_valve <- max(
      remaining_time - distances_to_closed_valves[[next_valve_to_open]] - 1,
      0
    )

    gain_from_next <- remaining_time_after_opening_valve * valve_flows[[next_valve_to_open]]
    flow_gain_from_rest <- m_get_max_flow(
      next_valve_to_open,
      setdiff(closed_valves, next_valve_to_open),
      remaining_time_after_opening_valve
    )
    return(gain_from_next + flow_gain_from_rest)
  }) |> max()
}

m_get_max_flow <- memoise(get_max_flow)

m_get_max_flow("AA", names(valve_flows), 30)
