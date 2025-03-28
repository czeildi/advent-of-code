library(tidyverse)
library(lubridate)
library(patchwork)
library(ggrepel)

# prepare data ------------------------------------------------------------
PERSON_STAR_LIMIT <- 25

daily_publish_times <- (seq.Date(as.Date("2020-12-01"), as.Date("2020-12-25"), by = "1 day") +
  lubridate::duration(6, 'hours')) %>%
  set_names(seq_along(.)) %>%
  enframe("day_idx", "publish_ts")

stats <- jsonlite::read_json("leaderboard_data/private_1226.json") %>%
  .[['members']] %>%
  discard(~ is.null(.[["name"]])) %>%
  discard(~ .[['local_score']] == 0) %>%
  set_names(map_chr(., "name"))

part_completions <- stats %>%
  map("completion_day_level") %>%
  enframe("name", "completion_day_level") %>%
  rowwise() %>%
  mutate(
    completion_day_level = list(enframe(completion_day_level, "day_idx", "part_completions"))
  ) %>%
  unnest(completion_day_level) %>%
  rowwise() %>%
  mutate(part_completions = list(enframe(part_completions, "part_idx", "completion_ts"))) %>%
  unnest(part_completions) %>%
  mutate(completion_ts = as_datetime(as.integer(map_chr(completion_ts, "get_star_ts")))) %>%
  left_join(daily_publish_times, by = "day_idx") %>%
  mutate(completion_seconds = as.integer(completion_ts - publish_ts) + 3600) %>%
  add_count(name, name = 'n_star')

# analysis ----------------------------------------------------------------

diff_between_parts_stat <- part_completions %>%
  add_count(name, day_idx) %>%
  filter(n == 2) %>%
  group_by(name, n_star, day_idx) %>%
  summarize(diff_between_parts = max(completion_seconds) - min(completion_seconds)) %>%
  ungroup() %>%
  filter(n_star >= PERSON_STAR_LIMIT)

diff_between_parts_stat %>%
  mutate(name = fct_reorder(name, diff_between_parts, .fun = median)) %>%
  ggplot(aes(x = name, y = diff_between_parts)) +
  geom_point(size = 3, alpha = 0.2) +
  geom_text_repel(
    data = filter(diff_between_parts_stat, diff_between_parts >= 12 * 3600),
    aes(label = day_idx)
  ) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 6, color = "red") +
  stat_summary(fun = median, geom = "point", shape = 21, size = 6, color = "darkgreen") +
  scale_y_sqrt(
    breaks = c(600, 3600, 6 * 3600, 24 * 3600),
    labels = c('10min', '1h', '6h', '1day')
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = 'time between solving part1 and part2',
    caption = "Green:  median\nRed: average"
  ) +
  theme_bw()

plot_completion <- function(df) {
  df %>%
    mutate(name = fct_reorder(name, completion_seconds)) %>%
    ggplot(aes(x = name, y = completion_seconds)) +
    geom_point(size = 3, alpha = 0.2) +
    stat_summary(fun = mean, geom = "point", shape = 21, size = 6, color = "red") +
    stat_summary(fun = median, geom = "point", shape = 21, size = 6, color = "darkgreen") +
    scale_y_sqrt(
      breaks = c(600, 3600, 6 * 3600, 24 * 3600),
      labels = c('10min', '1h', '6h', '1day')
    ) +
    coord_flip() +
    labs(x = NULL, y = 'time to solve', caption = "Green:  median\nRed: average") +
    theme_bw()
}

plot_completion(part_completions %>% filter(n_star >= PERSON_STAR_LIMIT & part_idx == 1)) +
  plot_completion(part_completions %>% filter(n_star >= PERSON_STAR_LIMIT & part_idx == 2)) +
  plot_annotation(tag_levels = 1)

part_completions %>%
  filter(n_star >= PERSON_STAR_LIMIT) %>%
  group_by(name, part_idx) %>%
  summarize(median_completion_seconds = median(completion_seconds)) %>%
  group_by(part_idx) %>%
  mutate(
    rank_for_part = dplyr::min_rank(median_completion_seconds),
    part_idx = as.character(part_idx)
  ) %>%
  ggplot(aes(x = part_idx, y = rank_for_part)) +
  geom_line(aes(group = name, color = name)) +
  geom_point(aes(color = name)) +
  geom_text(aes(x = part_idx, y = rank_for_part, label = rank_for_part), nudge_x = 0.03) +
  theme_minimal() +
  scale_y_continuous(breaks = 1:10)

diff_between_parts_stat %>%
  mutate(day_idx = fct_reorder(day_idx, diff_between_parts, .fun = median)) %>%
  ggplot(aes(x = day_idx, y = diff_between_parts)) +
  geom_point(size = 3, alpha = 0.2) +
  # geom_text_repel(
  #   data = filter(diff_between_parts_stat, diff_between_parts >= 12 * 3600),
  #   aes(label = name)
  # ) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 6, color = "red") +
  stat_summary(fun = median, geom = "point", shape = 21, size = 6, color = "darkgreen") +
  scale_y_sqrt(
    breaks = c(600, 3600, 6 * 3600, 24 * 3600),
    labels = c('10min', '1h', '6h', '1day')
  ) +
  labs(
    x = NULL,
    y = 'time between solving part1 and part2',
    caption = "Green:  median\nRed: average"
  ) +
  theme_bw()
