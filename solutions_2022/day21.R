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

# part 1
for (expr in exprs_in_topo_order) {
  eval(parse(text = expr))
}
root

# part 2

parsed <- input |>
  extract(x, c("name", "yell"), regex = "(.*): (.*)") |>
  mutate(
    node_id = row_number(),
    expr = paste(name, yell, sep = " <- ")
  ) |>
  mutate(yell = case_when(
    name == "humn" ~ "humn",
    name == "root" ~ sub("\\+", "-", yell),
    TRUE ~ yell
  ))

nodes <- select(parsed, node_id, name)
edges <- parsed |>
  filter(is.na(as.integer(yell))) |>
  separate_rows(yell) |>
  select(from = name, to = yell)

g <- tbl_graph(nodes, edges)

nodes_in_topo_order <- g |>
  activate(nodes) |>
  mutate(topo_order = node_topo_order()) |>
  activate(nodes) |>
  as_tibble() |>
  arrange(topo_order)

exprs_in_topo_order <- nodes_in_topo_order |>
  left_join(parsed, by = c("node_id", "name")) |>
  arrange(topo_order) |>
  select(name, yell) |>
  deframe()

library(symengine)

vars <- lapply(parsed$name, as.name)
names(vars) <- parsed$name
do.call(use_vars, vars)

expr <- S(exprs_in_topo_order[["root"]])

for (var_name in names(exprs_in_topo_order)) {
  expr <- subs(expr, S(var_name), S(exprs_in_topo_order[[var_name]]))
  expr
}

solve(expr, "humn")
