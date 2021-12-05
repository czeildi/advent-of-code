secret <- "ckczppom"

number <- 0

while(!startsWith(digest::digest(paste0(secret, number), "md5", serialize = FALSE), "000000")) {
  if (number %% 10000  == 0) {
    print(number)
  }
  number <- number + 1
}

number
