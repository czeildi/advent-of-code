library(tidyverse)
library(glue)
library(tidygraph)
options(scipen = 999)

year <- "2022"
day <- "21"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))
parsed <- input |>
  extract(x, c("name", "yell"), regex = "(.*): (.*)") |>
  mutate(
    node_id = row_number(),
    expr = paste(name, yell, sep = " <- ")
  )

nodes <- select(parsed, node_id, name)
edges <- parsed |>
  filter(is.na(as.integer(yell))) |>
  separate_rows(yell) |>
  select(from = yell, to = name)

g <- tbl_graph(nodes, edges)

nodes_in_topo_order <- g |>
  activate(nodes) |>
  mutate(topo_order = node_topo_order()) |>
  activate(nodes) |>
  as_tibble() |>
  arrange(topo_order)

exprs_in_topo_order <- nodes_in_topo_order |>
  left_join(parsed, by = "node_id") |>
  select(topo_order, expr) |>
  arrange(topo_order) |>
  deframe()

for (expr in exprs_in_topo_order) {
  eval(parse(text = expr))
}
root
