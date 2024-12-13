library(dplyr)

input_filename <- "input.txt"
input <- readLines(input_filename)
num_machines <- ceiling(length(input) / 4)

inds <- gregexpr("\\d+", input)
nums <- regmatches(input, inds) %>%
  unlist() %>%
  as.numeric()

tol <- 1e-3
cost <- 0

for (i in seq_len(num_machines)) {
  j <- 1 + 6 * (i - 1)
  a <- matrix(nums[j : (j + 3)], nrow = 2)
  b <- matrix(nums[(j + 4) : (j + 5)]) + 1e+13
  x <- solve(a, b)
  flag <- lapply(x, function(y) min(abs(c(y %% 1, y %% 1 - 1))) < tol) %>%
    unlist() %>%
    sum() == 2
  if (flag) {
    cost <- cost + 3 * x[1] + x[2]
  }
}

print(sprintf("%1.f", cost))