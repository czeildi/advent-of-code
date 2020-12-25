library(tidyverse)

card_public <- 8458505
door_public <- 16050997


# test
# card_public <- 5764801
# door_public <- 17807724

x <- 7
loop_idx <- 1

while(x != card_public && x != door_public) {
  loop_idx <- loop_idx + 1
  if (loop_idx %% 100000 == 0) print(loop_idx)
  x <- (x * 7) %% 20201227
}

if (x == card_public) {
  key <- door_public
  multiplier <- door_public
} else {
  key <- card_public
  multiplier <- card_public
}
todo_loop_num <- loop_idx

loop_idx <- 1
while(loop_idx < todo_loop_num) {
  loop_idx <- loop_idx + 1
  if (loop_idx %% 100000 == 0) print(loop_idx)
  key <- (key * multiplier) %% 20201227
}

key
