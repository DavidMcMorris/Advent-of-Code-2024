input_filename <- "input.txt"
input <- read.table(input_filename)[[1]]
gridsize <- nchar(input[1])

num_xmas <- sum(unlist(lapply(gregexpr("XMAS", input), length)))
num_samx <- sum(unlist(lapply(gregexpr("SAMX", input), length)))