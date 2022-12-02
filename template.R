library(tidyverse)
library(glue)
library(memoise)
options(scipen = 999)

year <- "2022"
day <- "01"
input_file <- glue("solutions_{year}/day{day}_input.txt")

# keep appropriate alternative
input <- read_file(input_file)
input <- tibble(x = read_lines(input_file))
input <- scan(input_file, sep = ",")
