library(tidyverse)
library(glue)
library(memoise)
options(scipen = 999)
library(ivs)

year <- "2022"
day <- "15"
input_file <- glue("solutions_{year}/day{day}_input.txt")
input <- tibble(x = read_lines(input_file))

sensors_and_beacons <- input |>
  extract(
    x, c("xs", "ys", "xb", "yb"),
    "Sensor at x=([0-9-]+), y=([0-9-]+): closest beacon is at x=([0-9-]+), y=([0-9-]+)",
    convert = TRUE
  ) |>
  mutate(manh = abs(xs - xb) + abs(ys - yb))

# part 1
target_y <- 2000000

sensors_and_beacons |>
  mutate(dist_y = abs(ys - target_y)) |>
  mutate(
    left = if_else(manh - dist_y < 0, NA_real_, xs - (manh - dist_y)),
    right = if_else(manh - dist_y < 0, NA_real_, xs + (manh - dist_y) + 1)
  ) |>
  filter(!is.na(left)) |>
  mutate(target_iv = iv(left, right)) |>
  summarise(union_intervals = iv_groups(target_iv)) |>
  mutate(s = iv_start(union_intervals), e = iv_end(union_intervals)) |>
  mutate(l = e - s - 1) |>
  pull(l) |>
  sum()

# part 2

xs <- sensors_and_beacons$xs
ys <- sensors_and_beacons$ys
manh <- sensors_and_beacons$manh
grid_max <- 4000000

system.time(
res <- invisible(lapply(0:grid_max, function(grid_y) {
  if (grid_y %% 10000 == 0) message(Sys.time(), ": ", grid_y)

  dist_y <- abs(ys - grid_y)
  left <- if_else(manh - dist_y < 0, NA_real_, pmax(xs - (manh - dist_y), 0))
  right <- if_else(manh - dist_y < 0, NA_real_, pmin(xs + (manh - dist_y) + 1, grid_max + 1))
  valid <- !is.na(left) & !is.na(right) & left < right

  ui <- iv_groups(iv(left[valid], right[valid]))

  if (length(ui) == 1) return(NULL)

  distress <- iv_start(iv_complement(ui))
  result <- distress * 4000000 + grid_y
  cat("\n\nResult:", result, "\n\n")
  result
}))
)

compact(res)[[1]]
