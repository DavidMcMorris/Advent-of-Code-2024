input_filename <- "sample.txt"
input <- read.table(input_filename)[[1]]
gridsize <- nchar(input[1])

num_xmas <- sum(gregexpr("XMAS", input) != -1)
num_samx <- sum(gregexpr("SAMX", input) != -1)
