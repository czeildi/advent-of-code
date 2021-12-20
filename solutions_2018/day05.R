library(tidyverse)

input <- read_lines("solutions_2018/input05.txt")

reactions <- rep("", 2 * 26)
names(reactions) <- c(paste0(letters, LETTERS), paste0(LETTERS, letters))

repeat {
  after_reduction <- str_replace_all(input, reactions)
  if (input == after_reduction) break;
  input <- after_reduction
}

nchar(input)

# part 2

map_int(1:26, ~{
  letter_regex <- c("", "")
  names(letter_regex) <- c(letters[.], LETTERS[.])
  without_letter <- str_replace_all(input, letter_regex)
  repeat {
    after_reduction <- str_replace_all(without_letter, reactions)
    if (without_letter == after_reduction) break;
    without_letter <- after_reduction
  }
  
  nchar(without_letter)
}) %>% 
  min()