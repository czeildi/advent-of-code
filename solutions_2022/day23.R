library(tidyverse)
library(glue)
library(ggplot2)
source("utils.R")
options(scipen = 999)

year <- "2022"
day <- "23"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- read_file(input_file)
elfs <- read_as_grid(input) |>
  filter(value == "#") |>
  mutate(y = -y) # so that y increases toward north

dirs <- as_tibble(data.table::fread("
  f,dir,dx,dy,mx,my
  n,nw,-1,1,0,1
  n,n,0,1,0,1
  n,ne,1,1,0,1
  e,ne,1,1,1,0
  e,e,1,0,1,0
  e,se,1,-1,1,0
  s,se,1,-1,0,-1
  s,s,0,-1,0,-1
  s,sw,-1,-1,0,-1
  w,sw,-1,-1,-1,0
  w,w,-1,0,-1,0
  w,nw,-1,1,-1,0
"))

make_move <- function(elfs, face_preference) {
  elfs_w_neighbors <- elfs |>
    inner_join(dirs, by = character()) |>
    mutate(nx = x + dx, ny = y + dy) |>
    left_join(elfs, by = c("nx" = "x", "ny" = "y")) |>
    rename(value = value.x, nb_value = value.y) |>
    mutate(has_elf = !is.na(nb_value)) |>
    select(x, y, f, has_elf, mx, my)

  happy_in_place <- elfs_w_neighbors |>
    group_by(x, y) |>
    summarize(is_free = !any(has_elf), .groups = "drop") |>
    filter(is_free) |>
    select(x, y)
  cannot_move <- elfs_w_neighbors |>
    group_by(x, y, f, mx, my) |>
    summarize(is_free = !any(has_elf), .groups = "drop") |>
    group_by(x, y) |>
    summarize(can_move = any(is_free), .groups = "drop") |>
    filter(!can_move) |>
    select(x, y) |>
    anti_join(happy_in_place, by = c("x", "y"))
  moving <- elfs_w_neighbors |>
    anti_join(happy_in_place, by = c("x", "y")) |>
    group_by(x, y, f, mx, my) |>
    summarize(is_free = !any(has_elf), .groups = "drop") |>
    filter(is_free) |>
    mutate(preference = match(f, face_preference)) |>
    group_by(x, y) |>
    summarize(
      mx = first(mx, order_by = preference),
      my = first(my, order_by = preference),
      .groups = "drop"
    ) |>
    mutate(tx = x + mx, ty = y + my) |>
    group_by(tx, ty) |>
    mutate(n_elf = n()) |>
    ungroup() |>
    mutate(x = if_else(n_elf == 1, tx, x), y = if_else(n_elf == 1, ty, y)) |>
    select(x, y) |>
    anti_join(happy_in_place, by = c("x", "y"))
  rbind(happy_in_place, moving) |> rbind(cannot_move) |> mutate(value = "#") |>
    arrange(x, y)
}

rotate <- function(face_preference) {
  lead(face_preference, default = face_preference[1])
}

face_preference <- c("n", "s", "w", "e")

# part 1
# for (i in seq_len(10)) {
#   elfs <- make_move(elfs, face_preference)
#   ggplot(elfs, aes(x, y)) + geom_tile() + coord_fixed()
#   face_preference <- rotate(face_preference)
# }

# elfs
# summarize(elfs, v = (max(x) - min(x) + 1) * (max(y) - min(y) + 1) - nrow(elfs))

# part 2
i <- 1
repeat {
  new_elfs <- make_move(elfs, face_preference)
  if (isTRUE(all.equal(elfs, new_elfs))) break
  elfs <- new_elfs
  face_preference <- rotate(face_preference)
  if (i %% 10 == 0) cat(i, "\n")
  i <- i + 1
}

i
