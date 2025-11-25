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

blinkn <- function(stone,n) {
    x <- list()
    new_stones <- blink(stone)
    if(n == 1){
        return(new_stones)
    } else {
        next_stones <- NULL
        for(j in seq_along(new_stones)) {
            next_stones <- c(next_stones,blinkn(new_stones[j],n-1))
        }
    }
    return(next_stones)
}