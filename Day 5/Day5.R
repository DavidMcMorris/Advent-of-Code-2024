library(dplyr)

input_filename <- "input.txt"
input <- readLines(input_filename)
linebreak <- which(input == "")
rules <- input[1:(linebreak - 1)] %>%
  strsplit(., split = "|", fixed = TRUE) %>%
  unlist() %>%
  as.numeric() %>%
  matrix(., ncol = 2, byrow = TRUE) %>%
  data.frame()

pages <- input[(linebreak + 1):length(input)] %>%
  strsplit(., split = ",", fixed = TRUE) %>%
  lapply(., as.numeric)

compare <- function(x1, x2) {
  x1sub <- subset(rules, X1 == x1)
  if (x2 %in% x1sub$X2) {
    return(1)
  } else {
    return(0)
  }
}

page_check <- function(page_order) {
  len <- length(page_order)
  for (i in 1:(len - 1)) {
    for (j in (i + 1):len) {
      flag <- compare(page_order[i], page_order[j])
      if (flag == 0) {
        return(flag)
      }
    }
  }
  return(flag)
}

total <- 0
for (i in seq_along(pages)) {
  flag <- page_check(pages[[i]])
  if (flag == 1) {
    total <- total + pages[[i]][(length(pages[[i]]) + 1) / 2]
  }
}

print(total)