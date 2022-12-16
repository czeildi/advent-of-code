library(tidyverse)
library(glue)
library(memoise)
library(tidygraph)
options(scipen = 999)

year <- "2022"
day <- "16"
input_file <- glue("solutions_{year}/day{day}_input.txt")

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

pairwise_distances <- do.call(rbind, lapply(valves$idx, function(to_id) {
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

  sapply(closed_valves, function(next_valve_to_open) {
    remaining_time_after_opening_valve <- max(
      remaining_time - distances[[current_position]][[next_valve_to_open]] - 1,
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

system.time(
max_flow <- m_get_max_flow("AA", names(valve_flows), 30)
)
max_flow


# part 2

neighbors <- select(parsed, name, neighbor) |>
  separate_rows(neighbor) |>
  inner_join(valves, by = c("neighbor" = "name")) |>
  group_by(name) |>
  summarize(neighbors = list(neighbor)) |>
  deframe()

get_max_flow_2 <- function(
  current_person,
  current_elephant,
  closed_valves,
  remaining_time
) {
  if (remaining_time <= 0) return(0)
  if (length(closed_valves) == 0) return(0)

  total_flow <- 0
  # both move
  for (next_person in neighbors[[current_person]]) {
    for (next_elephant in neighbors[[current_elephant]]) {
      gain_from_rest <- m_get_max_flow_2(
        next_person,
        next_elephant,
        closed_valves,
        remaining_time - 1
      )
      total_flow <- max(total_flow, gain_from_rest)
    }
  }
  # person moves, elephant opens
  if (current_elephant %in% closed_valves) {
    gain_from_current <- max(remaining_time - 1, 0) * valve_flows[[current_elephant]]
    for (next_person in neighbors[[current_person]]) {
      gain_from_rest <- m_get_max_flow_2(
        next_person,
        current_elephant,
        setdiff(closed_valves, current_elephant),
        remaining_time - 1
      )
      total_flow <- max(total_flow, gain_from_rest + gain_from_current)
    }
  }

  # person opens, elephant moves
  if (current_person %in% closed_valves) {
    gain_from_current <- max(remaining_time - 1, 0) * valve_flows[[current_person]]
    for (next_elephant in neighbors[[current_elephant]]) {
      gain_from_rest <- m_get_max_flow_2(
        current_person,
        next_elephant,
        setdiff(closed_valves, current_person),
        remaining_time - 1
      )
      total_flow <- max(total_flow, gain_from_rest + gain_from_current)
    }
  }
  # both open (unless they are both at the same position)
  if (current_person %in% closed_valves && current_elephant %in% closed_valves && current_person != current_elephant) {
    gain_from_current_e <- max(remaining_time - 1, 0) * valve_flows[[current_elephant]]
    gain_from_current_p <- max(remaining_time - 1, 0) * valve_flows[[current_person]]
    gain_from_rest <- m_get_max_flow_2(
      current_person,
      current_elephant,
      setdiff(closed_valves, c(current_person, current_elephant)),
      remaining_time - 1
    )
    total_flow <- max(total_flow, gain_from_rest + gain_from_current_e + gain_from_current_p)
  }

  total_flow
}

mf <- memoise(f, cache = cm)
m_get_max_flow_2 <- memoise(get_max_flow_2, cache = cachem::cache_mem(max_size = 5000 * 1024^2))

system.time(
max_flow <- m_get_max_flow_2("AA", "AA", names(valve_flows), 26)
)
max_flow
