library(tidyverse)
library(glue)
library(memoise)
options(scipen = 999)
library(lpSolve)

year <- "2022"
day <- "19"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))
blueprints <- input |>
  extract(x, c("id", "cost"), "Blueprint (\\d+): (.*)", convert = TRUE) |>
  separate_rows(cost, sep = "\\. ") |>
  extract(cost, c("target_type", "costs"), "Each (.*) robot costs ([a-z0-9 ]+)\\.?") |>
  separate_rows(costs, sep = " and ") |>
  extract(costs, c("cost", "cost_type"), "(\\d+) ([a-z]+)", convert = TRUE) |>
  nest_by(id, .keep = TRUE) |>
  deframe()

production <- function(bought, now) {
  max(now - bought - 1, 0)
}

max_t <- 24

res <- 0

# approach: linear programming with binary constraint on variables
# variables: for each resource type and minute, 1 if we buy a new robot of that type
#  at the start of given minute
# constraints:
#  - every variable has value 0 or 1
#  - at each minute for each resource type after buying the robot we planned for that minute
#    our remaining budget should be non-negative (produced until now - previously spent on buying robots)
#  - at each minute the sum of bought robots is at most 1

# ore is 1st, clay is 2nd, obsidian is 3rd, geode is 4th resource type
lapply(seq_along(blueprints), function(blueprint_id) {

  bp <- blueprints[[blueprint_id]]$cost
  # 4x4 matrix, m[i, j]: - cost of resource j payed in resource type i
  costs <- rbind(
    c(-bp[1], -bp[2], -bp[3], -bp[5]),
    c(0, 0, -bp[4], 0),
    c(0, 0, 0, -bp[6]),
    c(0, 0, 0, 0)
  )

  # 4 * max_t x 4 * max_t matrix of coefficients
  #
  # m[4 * (t - 1) + i, 4 * (s - 1) +j]: at the t. minute, having bought 1 unit of type j in the previous s. minute
  # has a production-cost of resource type i
  # 
  # columns correspond to ore buy in 1st minute, clay buy in 1st minute, ... obsidian buy in 24th minute, geode buy in 24th minute
  # 
  # rows correspond to positivity constraint on ore stock in 1st minute, on clay stock in first minute, ..., positivity constraint on geode stock in 24th minute
  has_money_for_buying <- lapply(seq_len(max_t), function(now) {
    lapply(seq_len(max_t), function(bought) {
      # the future does not affect us
      if (bought > now) return(diag(0, 4, 4))
      costs + diag(production(bought, now), 4, 4)
    }) |> do.call(cbind, args = _)
  }) |> do.call(rbind, args = _)

  # columns correspond to ore buy in 1st minute, clay buy in 1st minute, ... obsidian buy in 24th minute, geode buy in 24th minute
  #
  # 1 row for each minute: in that minute we can only buy at most 1 robot in total
  one_robot_per_minute <- lapply(seq_len(max_t), function(now) {
    lapply(seq_len(max_t), function(t) {
      if (t != now) return(matrix(c(0, 0, 0, 0), nrow = 1))
      return(matrix(c(-1, -1, -1, -1), nrow = 1))
    }) |> do.call(cbind, args = _)
  }) |> do.call(rbind, args = _)

  lp_coeffs <- rbind(has_money_for_buying, one_robot_per_minute)

  # our initial ore robot produces t-1 ore until the t. minute for free
  # for each other resource types we have to pay ourselves
  lp_lower_bounds <- lapply(seq_len(max_t), function(t) c(-t + 1, 0, 0, 0)) |>
    do.call(c, args = _)
  lp_lower_bounds <- c(lp_lower_bounds, rep(-1, max_t))

  # we are interested in total geode production
  max_coeffs <- lapply(seq_len(max_t), function(t) c(0, 0, 0, max_t - t)) |>
    do.call(c, args = _)

  # in each minute, 1 constraint for each resource type non-negativity and 1 constraint that we can only build 1 robot
  n_constraints <- 4 * max_t + max_t

  max_geode <- lp(
    "max",
    max_coeffs, lp_coeffs, rep(">=", n_constraints), lp_lower_bounds,
    all.bin = TRUE
  )

  cat(blueprint_id, ": ", max_geode$objval, "", as.character(Sys.time()), "\n")
  res <<- res + blueprint_id * max_geode$objval
})

res
