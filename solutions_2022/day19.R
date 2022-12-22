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

lapply(seq_along(blueprints), function(blueprint_id) {

  bp <- blueprints[[blueprint_id]]$cost
  costs <- rbind(
    c(-bp[1], -bp[2], -bp[3], -bp[5]),
    c(0, 0, -bp[4], 0),
    c(0, 0, 0, -bp[6]),
    c(0, 0, 0, 0)
  )

  has_money_for_buying <- lapply(seq_len(max_t), function(now) {
    lapply(seq_len(max_t), function(bought) {
      # the future does not affect us
      if (bought > now) return(diag(0, 4, 4))
      costs + diag(production(bought, now), 4, 4)
    }) |> do.call(cbind, args = _)
  }) |> do.call(rbind, args = _)

  one_robot_per_minute <- lapply(seq_len(max_t), function(now) {
    lapply(seq_len(max_t), function(t) {
      if (t != now) return(matrix(c(0, 0, 0, 0), nrow = 1))
      return(matrix(c(-1, -1, -1, -1), nrow = 1))
    }) |> do.call(cbind, args = _)
  }) |> do.call(rbind, args = _)

  lp_coeffs <- rbind(has_money_for_buying, one_robot_per_minute)

  lp_lower_bounds <- lapply(seq_len(max_t), function(t) c(-t + 1, 0, 0, 0)) |>
    do.call(c, args = _)
  lp_lower_bounds <- c(lp_lower_bounds, rep(-1, max_t))

  max_coeffs <- lapply(seq_len(max_t), function(t) c(0, 0, 0, max_t - t)) |>
    do.call(c, args = _)

  max_geode <- lp(
    "max",
    max_coeffs, lp_coeffs, rep(">=", max_t * 5), lp_lower_bounds,
    all.bin = TRUE
  )

  cat(blueprint_id, ": ", max_geode$objval, "", as.character(Sys.time()), "\n")
  res <<- res + blueprint_id * max_geode$objval
})

res
