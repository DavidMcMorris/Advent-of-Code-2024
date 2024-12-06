library(dplyr)

input_filename <- "input.txt"
input <- read.table(input_filename)[[1]]
gridsize <- nchar(input[1])

input <- input %>%
  strsplit(., "") %>%
  unlist() %>%
  matrix(., nrow = gridsize, byrow = TRUE)

obstructions <- arrayInd(which(input == "O"), rep(gridsize, 2))
direction <- matrix(c(-1, 0))
guard <- matrix(arrayInd(which(input == "^"), rep(gridsize, 2)))
turn_mat <- matrix(c(0, -1, 1, 0), nrow = 2)

coord_tester <- function(mat) {
  sum(mat <= 0) == 0 && sum(mat > gridsize) == 0
}

walk <- function(guard, direction) {
  next_step <- guard + direction
  if (paste0(next_step, collapse = "") %in% apply(obstructions, 1, paste0, collapse = "")) {
    direction <- turn_mat %*% direction
  } else {
    guard <- next_step
  }
  return(list(guard, direction))
}

flag <- 0
steps <- 0
output <- list(guard, direction)
loc_hist <- paste0(guard, collapse = "")
while (flag == 0) {
  output <- walk(output[[1]], output[[2]])
  if (!coord_tester(output[[1]])) {
    flag <- 1
  } else {
    steps <- steps + 1
    loc_hist <- c(loc_hist, paste0(output[[1]], collapse = ""))
  }
}

print(length(unique(loc_hist)))