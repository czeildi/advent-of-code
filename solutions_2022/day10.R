library(tidyverse)
library(glue)
library(memoise)
options(scipen = 999)

year <- "2022"
day <- "10"
input_file <- glue("solutions_{year}/day{day}_input.txt")

input <- tibble(x = read_lines(input_file))

commands <- input |>
  separate(x, into = c("cmd", "dx"), sep = " ", convert = TRUE, fill = "right") |>
  mutate(idx = cumsum(if_else(cmd == "addx", 2L, 1L)) + 1) |>
  mutate(dx = if_else(is.na(dx), 0L, dx))

dummy <- commands |>
  filter(cmd == "addx") |>
  mutate(dx = 0, idx = idx - 1)

# part 1
rbind(commands, dummy) |>
  arrange(idx) |>
  mutate(value = cumsum(dx) + 1) |>
  filter(idx %% 40 == 20) |>
  summarize(sum(value * idx))

# part 2
rbind(commands, dummy) |>
  arrange(idx) |>
  mutate(x = (row_number() - 1) %% 40, y = (row_number() - 1) %/% 40) |>
  mutate(value = cumsum(dx) + 1) |>
  filter(abs(x - lag(value, default = 1)) <= 1) |>
  ggplot(aes(x, -y)) + geom_tile()
