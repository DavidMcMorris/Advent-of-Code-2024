library(dplyr)

input_filename <- "input.txt"
input <- readLines(input_filename)

mul_inds <- gregexpr("mul\\(\\K\\d+,\\d+(?=\\))", input, perl = TRUE)
muls <- regmatches(input, mul_inds)[[1]]
mul_inds <- as.vector(mul_inds[[1]])
dos <- as.vector(gregexpr("do\\(\\)", input, perl = TRUE)[[1]])
donts <- as.vector(gregexpr("don't\\(\\)", input, perl = TRUE)[[1]])

muls <- muls %>%
  strsplit(., ",") %>%
  unlist() %>%
  as.numeric() %>%
  matrix(., ncol = 2, byrow = TRUE)

pt1_mul_sum <- sum(muls[, 1] * muls[, 2])
print(pt1_mul_sum)

current_instruction <- 1
pt2_mul_sum <- 0
for (i in sort(c(mul_inds, dos, donts))) {
  if (i %in% dos) {
    current_instruction <- 1
  } else if (i %in% donts) {
    current_instruction <- 0
  } else {
    j <- which(mul_inds == i)
    pt2_mul_sum <- pt2_mul_sum + current_instruction * (muls[j, 1] * muls[j, 2])
  }
}

print(pt2_mul_sum)