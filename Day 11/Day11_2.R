library(dplyr)

input_filename <- "input.txt"
input <- scan(input_filename)
stone_cache <- list()

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

blinkn_len <- function(stone,n) {
    if(is.null(stone_cache[[as.character(stone)]])) {
        stone_cache[[as.character(stone)]][n] <<- blinkn(stone,n) %>% length()
        return(stone_cache[[as.character(stone)]][n])
    } else if(!is.na(stone_cache[[as.character(stone)]][n])) {
        return(stone_cache[[as.character(stone)]][n])
    } else {
        stone_cache[[as.character(stone)]][n] <<- blinkn(stone,n) %>% length()
        return(stone_cache[[as.character(stone)]][n])
    }
}

len <- 0
for(i in seq_along(input)) {
  print(i)
  len <- len + blinkn_len(input[i],75)
}