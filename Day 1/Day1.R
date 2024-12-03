input_filename <- "input.txt"
input <- read.table(input_filename)

input <- apply(input, 2, sort)

distances <- sum(apply(input, 1, function(x) {abs(x[1] - x[2])}))

left_values <- unique(input[, 1])
counts <- matrix(nrow = length(left_values), ncol = 2)

for (i in seq_along(left_values)) {
  counts[i, 1] <- sum(input[, 1] == left_values[i])
  counts[i, 2] <- sum(input[, 2] == left_values[i])
}

similarity <- sum(left_values * counts[, 1] *  counts[, 2])

print(distances)
print(similarity)