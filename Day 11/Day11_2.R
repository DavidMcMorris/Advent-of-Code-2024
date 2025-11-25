library(dplyr)

input_filename <- "input.txt"
input <- scan(input_filename)
one <- NULL
zero <- NULL

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


input1 <- 1
extra <- 0
for(i in 1:75) {
  print(i)
  new_stones <- NULL
  for (j in seq_along(input1)) {
    if (j > 1 && input1[j] == 1) {
        extra <- extra + length(input1)
    } else {
        new_stones <- c(new_stones, blink(input1[j]))
        one[i] <- length(new_stones) + extra
    }
  }
  input1 <- new_stones
}

input0 <- 0
extra <- 0
for(i in 1:75) {
#   print(i)
  new_stones <- NULL
  for (j in seq_along(input0)) {
    if (input0[j] == 0) {
        extra <- extra + length(input0)
    } else {
        new_stones <- c(new_stones, blink(input0[j]))
        zero[i] <- length(new_stones)
    }
  }
  input0 <- new_stones + extra
}

onezero <- 0
for(i in 1:75){
  print(i)
  new_stones <- NULL
  for (j in seq_along(input)) {
    if(input[j] == 1 || input[j] == 1){
        if(input[j] == 1){
            onezero <- onezero + one[76-i]
        } else {
            onezero <- onezero + one[76-i]
        }
    } else {
    new_stones <- c(new_stones, blink(input[j]))
    }
  }
  input <- new_stones
}
