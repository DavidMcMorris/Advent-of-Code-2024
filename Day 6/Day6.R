library(dplyr)

input_filename <- "sample.txt"
input <- read.table(input_filename)[[1]]
gridsize <- nchar(input[1])

input <- input %>%
  strsplit(., "") %>%
  unlist() %>%
  matrix(., nrow = gridsize, byrow = TRUE)

obstructions <- arrayInd(which(input == "O"), rep(gridsize, 2))
direction <- c(-1, 0)
guard <- arrayInd(which(input == "^"), rep(gridsize, 2))
turn_mat <- matrix(c(0, -1, 1, 0), nrow = 2)

walk <- function(guard, direction) {
  if (guard + direction %in% obstructions) {
    direction <- turn_mat %*% direction
  } else {
    guard <- guard + direction
  }
  return(list(guard, direction))
}