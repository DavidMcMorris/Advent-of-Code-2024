library(dplyr)

input_filename <- "sample.txt"
input <- read.table(input_filename)[[1]]
gridsize <- nchar(input[1])

input <- input %>%
  strsplit(., "") %>%
  unlist() %>%
  matrix(., nrow = gridsize, byrow = TRUE)
