library(dplyr)

input_filename <- "input.txt"
input <- readLines(input_filename)

mulls <- regmatches(input,gregexpr("mul\\(\\K\\d+,\\d+(?=\\))", input, perl=TRUE))[[1]]

mulls <- mulls %>%
  strsplit(., ",") %>%
  unlist() %>%
  as.numeric() %>%
  matrix(., ncol = 2, byrow = TRUE)

mul_sum <- sum(mulls[, 1] * mulls[, 2])
print(mul_sum)