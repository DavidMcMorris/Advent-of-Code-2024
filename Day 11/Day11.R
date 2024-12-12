library(dplyr)

input_filename <- "input.txt"
input <- scan(input_filename)

blink <- function(stone) {
  if (stone == 0) {
    new_stones <- 1
  } else if (nchar(stone) %% 2 == 0) {
    mid <- (nchar(stone)) / 2
    digits <- as.character(stone) %>%
      strsplit(split = "") %>%
      unlist()
    new_stones <- digits[1:mid] %>%
      paste0(collapse = "") %>%
      as.numeric()
    new_stones <- digits[-(1:mid)]  %>%
      paste0(collapse = "") %>%
      as.numeric() %>%
      c(new_stones, .)
  } else {
    new_stones <- stone * 2024
  }
  return(new_stones)
}

for (i in 1:25) {
  print(i)
  new_stones <- NULL
  for (j in seq_along(input)) {
    new_stones <- c(new_stones, blink(input[j]))
  }
  input <- new_stones
}

print(length(input))