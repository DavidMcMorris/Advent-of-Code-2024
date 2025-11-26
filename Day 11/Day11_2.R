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
    lab <- paste(stone,n,sep="_")
    if(!is.null(stone_cache[[lab]])) {
      return(stone_cache[[lab]])
    } else {
      new_stones <- blink(stone)
      new_stones <- as.data.frame(table(new_stones)) %>% setNames(c("stone","freq"))
      if(n == 1){
          stone_cache[[lab]] <<- sum(new_stones$freq)
          return(stone_cache[[lab]])
      } else {
          len <- 0
          for(j in seq_along(new_stones$stone)) {
              next_stones <-  blinkn(as.numeric(as.character(new_stones$stone[j])),n-1)
              stone_cache[[paste(as.numeric(as.character(new_stones$stone[j])),n-1,sep="_")]] <<- next_stones
              len <- len + new_stones$freq[j] * next_stones
          }
      }
      stone_cache[[lab]] <<- len
      return(len)
    }
}


len <- 0
for(i in seq_along(input)) {
  print(i)
  len <- len + blinkn(input[i],75)
}
print(len)