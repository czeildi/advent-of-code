library(tidyverse)

passports <- tibble(x = read_file("solutions_2020/day04_input.txt")) %>% 
  separate_rows(x, sep = "\n\n") %>% 
  mutate(y = 1:nrow(.)) %>% 
  separate_rows(x, sep = "[ \n]+") %>% 
  filter(x != "") %>%
  extract(x, into = c('id', 'value'), regex = "([a-z]+):(.*)")


# part 1 ------------------------------------------------------------------

passports %>%
  filter(id != 'cid') %>%
  select(id, y) %>%
  distinct() %>%
  count(y) %>% 
  filter(n >= 7) %>% 
  nrow()

# part 2 ------------------------------------------------------------------


valid <- passports %>%
  filter(id != 'cid') %>% 
  pivot_wider(id_cols = y, names_from = id, values_from = value, values_fill = NA) %>% 
  drop_na() %>% 
  filter(byr >= 1920 & byr <= 2002) %>%
  filter(iyr >= 2010 & iyr <= 2020) %>%
  filter(eyr >= 2020 & eyr <= 2030) %>%
  filter((hgt >= '150cm' & hgt <= '193cm' & str_detect(hgt, '^[0-9]{3,3}cm$')) | (hgt >= '59in' & hgt <= '76in' & str_detect(hgt, '^[0-9]{2,2}in$'))) %>%
  filter(str_detect(hcl, '^#[0-9a-f]{6,6}$')) %>%
  filter(ecl %in% c('amb', 'blu', 'oth', 'hzl', 'grn', 'gry', 'brn')) %>%
  filter(str_detect(pid, '^[0-9]{9,9}$'))

nrow(valid)
