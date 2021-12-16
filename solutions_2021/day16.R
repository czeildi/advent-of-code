library(tidyverse)
library(glue)

decimal_to_binary <- function(num) {
  result <- num %% 2
  num <- (num - result) / 2
  while (num > 0) {
    result <- paste0(num %% 2, result)
    num <- (num - num %% 2) / 2
  }
  str_pad(result, 4, pad = '0')
}

hex_bits <- map_chr(0:15, decimal_to_binary)
names(hex_bits) <- c(0:9, LETTERS[1:6])

input <- read_lines("solutions_2021/input16.txt")
# input <- "A0016C880162017C3686B18A3D4780"

binary_input <- input %>% 
  str_split("") %>% 
  pluck(1) %>% 
  hex_bits[.] %>% 
  str_c(collapse = "")

read_packets <- function(binary_input) {
  if (nchar(binary_input) < 4) return(0)
  
  version <- strtoi(str_sub(binary_input, end = 3), 2)
  typeId <- strtoi(str_sub(binary_input, start = 4, end = 6), 2)
  
  if (typeId == 4) {
    rest <- str_sub(binary_input, start = 7)
    while(str_sub(rest, 1, 1) == "1") {
      rest <- str_sub(rest, 6)
    }
    rest <- str_sub(rest, 6)
    return(version + read_packets(rest))
  } else {
    lengthTypeId <- str_sub(binary_input, 7, 7)
    rest <- str_sub(binary_input, 8)
    if (lengthTypeId == "0") {
      if (nchar(rest) < 15) return(version)
      subPacketBits <- strtoi(str_sub(rest, 1, 15), 2)
      thisPacket <- str_sub(rest, 16, 16 + subPacketBits - 1)
      rest <- str_sub(rest, 16 + subPacketBits)
      return(version + read_packets(thisPacket) + read_packets(rest))
    } else {
      if (nchar(rest) < 11) return(version)
      numSubPacket <- strtoi(str_sub(rest, 1, 11), 2)
      rest <- str_sub(rest, 12)
      return(version + read_packets(rest))
    }
  }
}

read_packets(binary_input)
