library(tidyverse)

numbers <- as.numeric(readr::read_lines("solutions_2020/day01_input.txt"))

for (x in numbers) {
  for (y in numbers) {
    for(z in numbers) {
      if ((x + y + z) == 2020) {
        print(x*y*z) 
      }
    }
  }
}