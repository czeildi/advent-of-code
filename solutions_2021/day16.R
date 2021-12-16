library(tidyverse)

# helpers ----------------------------------------

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

hex_to_bin <- function(input) {
  input %>% 
    str_split("") %>% 
    pluck(1) %>% 
    hex_bits[.] %>% 
    str_c(collapse = "")
}

aggregate_packet_values <- function(type_id, packet_values) {
  if (type_id == 0 || type_id == 4) return(sum(packet_values))
  if (type_id == 1) return(prod(packet_values))
  if (type_id == 2) return(min(packet_values))
  if (type_id == 3) return(max(packet_values))
  if (type_id == 5) return(if_else(packet_values[1] > packet_values[2], 1, 0))
  if (type_id == 6) return(if_else(packet_values[1] < packet_values[2], 1, 0))
  if (type_id == 7) return(if_else(packet_values[1] == packet_values[2], 1, 0))
}

# parsing -------------------------------------------

read_packets <- function(binary_input, num_packets) {
  if (nchar(binary_input) < 4 || num_packets == 0) {
    return(list(values = numeric(0), text_to_parse = binary_input))
  }
  
  type_id <- strtoi(str_sub(binary_input, start = 4, end = 6), 2)
  
  if (type_id == 4) {
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
    length_type_id <- str_sub(binary_input, 7, 7)
    text_to_parse <- str_sub(binary_input, 8)
    
    if (length_type_id == "0") {
      n_bits_in_sub_packet <- strtoi(str_sub(text_to_parse, 1, 15), 2)
      current_packet <- str_sub(text_to_parse, 16, 16 + n_bits_in_sub_packet - 1)
      
      current_packet_values <- read_packets(current_packet, Inf)$values
      leftover <- str_sub(text_to_parse, 16 + n_bits_in_sub_packet)
    } else {
      n_sub_packet <- strtoi(str_sub(text_to_parse, 1, 11), 2)
      
      current_packets <- read_packets(str_sub(text_to_parse, 12), n_sub_packet)
      
      current_packet_values <- current_packets$values
      leftover <- current_packets$text_to_parse
    }
  }
  
  rest_packets <- read_packets(leftover, num_packets - 1)
  return(list(
    values = c(aggregate_packet_values(type_id, current_packet_values), rest_packets$values),
    text_to_parse = rest_packets$text_to_parse
  ))
}

# result ---------------------------------

options(scipen=99)
input <- read_lines("solutions_2021/input16.txt")
read_packets(hex_to_bin(input), num_packets = 1)$values