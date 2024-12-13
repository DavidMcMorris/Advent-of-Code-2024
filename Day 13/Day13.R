library(dplyr)

input_filename <- "input.txt"
input <- readLines(input_filename)
num_machines <- length(input)

inds <- gregexpr("\\d+", input)
nums <- regmatches(input, inds) %>%
  unlist() %>%
  as.numeric()

tol <- 1e-10
cost <- 0

for (i in seq_len(num_machines)) {
  a <- matrix(nums[i : (i + 3)], nrow = 2)
  b <- matrix(nums[(i + 4) : (i + 5)])
  x <- solve(a, b)
  if (sum(unlist(lapply(x, function(y) min(abs(c(y %% 1, y %% 1 - 1))) < tol))) == 2) {
    cost <- cost + 3 * x[1] + x[2]
  }
}

print(cost)