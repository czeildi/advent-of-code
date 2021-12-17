library(tidyverse)
library(unglue)
library(zeallot)

input <- "target area: x=14..50, y=-267..-225"
# input <- "target area: x=20..30, y=-10..-5" # sample

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

# part 1
positions %>%
  filter(x >= x1 & x <= x2 & y >= y1 & y <= y2) %>%
  select(vx, vy) %>%
  distinct() %>%
  inner_join(positions) %>%
  summarize(highest = max(y), .groups = "drop") %>%
  arrange(desc(highest)) %>%
  head(1)

# part 2
positions %>% 
  filter(x >= x1 & x <= x2 & y >= y1 & y <= y2) %>% 
  select(vx, vy) %>% 
  distinct() %>% 
  nrow()
