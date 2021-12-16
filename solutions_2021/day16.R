library(tidyverse)
library(glue)

decimal_to_4_digit_binary <- function(num) {
  result <- num %% 2
  num <- (num - result) / 2
  while (num > 0) {
    result <- paste0(num %% 2, result)
    num <- (num - num %% 2) / 2
  }
  str_pad(result, 4, pad = '0')
}

str_to_int <- function(text) {
  digits <- rev(as.numeric(str_split(text, "")[[1]]))
  sum(digits * 2^(0:(length(digits) - 1)))
}

hex_bits <- map_chr(0:15, decimal_to_4_digit_binary)
names(hex_bits) <- c(0:9, LETTERS[1:6])

aggregate_packet_values <- function(typeId, packet_values) {
  if (typeId == 0 || typeId == 4) {
    value <- sum(packet_values)
  } else if (typeId == 1) {
    value <- prod(packet_values)
  } else if (typeId == 2) {
    value <- min(packet_values)
  } else if (typeId == 3) {
    value <- max(packet_values)
  } else if (typeId == 5) {
    value <- if_else(packet_values[1] > packet_values[2], 1, 0)
  } else if (typeId == 6) {
    value <- if_else(packet_values[1] < packet_values[2], 1, 0)
  } else if (typeId == 7) {
    value <- if_else(packet_values[1] == packet_values[2], 1, 0)
  }
  return(value)
}

input <- read_lines("solutions_2021/input16.txt")
# input <- "C200B40A82"

binary_input <- input %>% 
  str_split("") %>% 
  pluck(1) %>% 
  hex_bits[.] %>% 
  str_c(collapse = "")

read_packets <- function(binary_input, num_packets) {
  if (nchar(binary_input) < 10) return(list(values = numeric(0), text_to_parse = ""))
  if (num_packets == 0) return(list(values = numeric(0), text_to_parse = binary_input))
  
  version <- strtoi(str_sub(binary_input, end = 3), 2)
  typeId <- strtoi(str_sub(binary_input, start = 4, end = 6), 2)
  
  if (typeId == 4) {
    text_to_parse <- str_sub(binary_input, start = 7)
    blocks <- character(0)
    while(str_sub(text_to_parse, 1, 1) == "1") {
      blocks <- c(blocks, str_sub(text_to_parse, 2, 5))
      text_to_parse <- str_sub(text_to_parse, 6)
    }
    blocks <- c(blocks, str_sub(text_to_parse, 2, 5))
    current_packet_values <- str_to_int(str_c(blocks, collapse = ""))
    leftover <- str_sub(text_to_parse, 6)
  } else {
    lengthTypeId <- str_sub(binary_input, 7, 7)
    text_to_parse <- str_sub(binary_input, 8)
    if (lengthTypeId == "0") {
      if (nchar(text_to_parse) < 15) return(list(values = numeric(0), rest = ""))
      subPacketBits <- strtoi(str_sub(text_to_parse, 1, 15), 2)
      thisPacket <- str_sub(text_to_parse, 16, 16 + subPacketBits - 1)
      leftover <- str_sub(text_to_parse, 16 + subPacketBits)
      rest_packets <- read_packets(thisPacket, 100) # infinity
      current_packet_values <- rest_packets$values
    } else {
      if (nchar(text_to_parse) < 11) return(list(values = numeric(0), rest = ""))
      numSubPacket <- strtoi(str_sub(text_to_parse, 1, 11), 2)
      rest_packets <- read_packets(str_sub(text_to_parse, 12), numSubPacket)
      current_packet_values <- rest_packets$values
      leftover <- rest_packets$text_to_parse
    }
  }
  if (leftover == "") {
    return(list(
      values = aggregate_packet_values(typeId, current_packet_values),
      text_to_parse = ""
    ))
  } else {
    rest_packets <- read_packets(leftover, num_packets - 1)
    return(list(values = c(
      aggregate_packet_values(typeId, current_packet_values),
      rest_packets$values
    ), text_to_parse = rest_packets$text_to_parse))
  }
}

options(scipen=99)
read_packets(binary_input, 1)$values
