library(tidyverse)
library(rvest)
library(glue)

year <- 2020
csv_filename <- glue("leaderboard_data/global_leaderboard_{year}.csv")
# leaderboard_pages <- map(1:25, ~read_html(glue("https://adventofcode.com/{year}/leaderboard/day/{.}")))
# 
# leaderboard_entries <- imap(leaderboard_pages, ~{
#   .x %>%
#     html_nodes('body') %>%
#     xml_find_all("//div[@class='leaderboard-entry']") %>%
#     html_text() %>%
#     tibble(entry = .) %>%
#     mutate(idx = 1:n()) %>%
#     extract(
#       entry,
#       into = c("place", "day_idx", "time", "user_name"),
#       regex = "(\\d+)\\) Dec (\\d+)  (\\d\\d:\\d\\d:\\d\\d)  (.*)"
#     ) %>%
#     mutate(day_idx = .y, type = if_else(idx %% 200 <= 100, "gold", "silver")) %>% 
#     select(-idx)
# }) %>%
#   reduce(rbind)
# 
# write_csv(leaderboard_entries, csv_filename)

leaderboard_entries <- read_csv(csv_filename) %>%
  mutate(day_idx = str_pad(as.character(day_idx), 2, pad = "0"))

n_distinct(leaderboard_entries$user_name)

theme_set(theme_bw())

ggplot(leaderboard_entries, aes(x = day_idx, y = time)) + 
  geom_boxplot() + 
  facet_wrap(vars(type), ncol = 1, scales = "free_y")

part_comparison <- inner_join(
  leaderboard_entries %>% filter(type == "silver"),
  leaderboard_entries %>% filter(type == "gold"),
  by = c("user_name", "day_idx")
)

ggplot(part_comparison, aes(x = place.x, y = place.y)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm")

ggplot(part_comparison, aes(x = day_idx, y = as.integer(time.y - time.x))) + 
  geom_boxplot()
