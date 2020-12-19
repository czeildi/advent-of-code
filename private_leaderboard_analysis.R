library(tidyverse)
library(lubridate)
library(patchwork)

daily_publish_times <- (seq.Date(as.Date("2020-12-01"), as.Date("2020-12-25"), by = "1 day") + lubridate::duration(6, 'hours')) %>%
  set_names(seq_along(.)) %>%
  enframe("day_idx", "publish_ts")

stats <- jsonlite::read_json("leaderboard_data/stats.json") %>% 
  .[['members']] %>%
  discard(~is.null(.[["name"]])) %>%
  discard(~.[['local_score']] == 0) %>%
  set_names(map_chr(., "name"))

part_completions <- stats %>%
  map("completion_day_level") %>%
  enframe("name", "completion_day_level") %>%
  rowwise() %>%
  mutate(completion_day_level = list(enframe(completion_day_level, "day_idx", "part_completions"))) %>%
  unnest(completion_day_level) %>%
  rowwise() %>%
  mutate(part_completions = list(enframe(part_completions, "part_idx", "completion_ts"))) %>%
  unnest(part_completions) %>%
  mutate(completion_ts = as_datetime(as.integer(map_chr(completion_ts, "get_star_ts")))) %>%
  left_join(daily_publish_times, by = "day_idx") %>%
  mutate(completion_seconds = as.integer(completion_ts - publish_ts) + 3600) %>%
  add_count(name, name = 'n_star')
  
diff_between_parts_stat <- part_completions %>%
  filter(n_star >= 20) %>%
  add_count(name, day_idx) %>%
  filter(n == 2) %>%
  group_by(name, n_star, day_idx) %>%
  summarize(diff_between_parts = max(completion_seconds) - min(completion_seconds))

diff_between_parts_stat %>%
  ggplot(aes(x = fct_reorder(name, diff_between_parts), y = diff_between_parts)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2, size = 3) + 
  scale_y_sqrt(
    breaks = c(600, 3600, 6 * 3600, 24*3600),
    labels = c('10min', '1h', '6h', '1day')
  ) + 
  coord_flip() + 
  labs(x = NULL, y = 'time between solving part1 and part2') + 
  theme_bw()

plot_completion <- function(df) {
  df %>%
    ggplot(aes(x = fct_reorder(name, completion_seconds), y = completion_seconds)) + 
    geom_boxplot(position = position_dodge(width = 0.9)) + 
    geom_point(alpha = 0.2, size = 3, position = position_dodge(width = 0.9)) + 
    scale_y_sqrt(
      breaks = c(600, 3600, 6 * 3600, 24*3600),
      labels = c('10min', '1h', '6h', '1day')
    ) + 
    coord_flip() + 
    labs(x = NULL, y = 'time to solve') + 
    theme_bw()
}

plot_completion(part_completions %>% filter(n_star >= 20 & part_idx == 1)) + 
  plot_completion(part_completions %>% filter(n_star >= 20 & part_idx == 2)) +
  plot_annotation(tag_levels = 1)

part_completions %>%
  filter(n_star >= 20) %>%
  ggplot(aes(x = fct_reorder(part_idx, completion_seconds), y = completion_seconds)) + 
  geom_violin() + 
  scale_y_sqrt(
    breaks = c(600, 3600, 6 * 3600, 24*3600),
    labels = c('10min', '1h', '6h', '1day')
  ) + 
  coord_flip() + 
  labs(x = NULL, y = 'time between publish and solving') + 
  theme_bw()
