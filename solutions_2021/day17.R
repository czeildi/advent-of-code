library(tidyverse)
library(unglue)
library(zeallot)

input <- "target area: x=14..50, y=-267..-225"

c(x1, x2, y1, y2) %<-% unglue_data(input, "target area: x={x1}..{x2}, y={y1}..{y2}", convert = TRUE)

positions <- crossing(vx = 0:50, vy= -267:500, s = 0:2000) %>% # range by trial and error
  mutate(
    dx = pmax(vx - s, 0),
    dy = vy - s
  ) %>% 
  group_by(vx, vy) %>% 
  mutate(
    x = cumsum(dx),
    y = cumsum(dy)
  ) 

correct_initial_velocities <- positions %>%
  filter(x >= x1 & x <= x2 & y >= y1 & y <= y2) %>%
  select(vx, vy) %>%
  distinct()

# part 1
correct_initial_velocities %>%
  inner_join(positions) %>%
  summarize(highest = max(y), .groups = "drop") %>%
  arrange(desc(highest)) %>%
  head(1)

# part 2
nrow(correct_initial_velocities)
