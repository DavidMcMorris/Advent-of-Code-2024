input_filename <- "input.txt"
input <- read.table(input_filename)[[1]]
gridsize <- nchar(input[1])

x <- gregexpr("X", input)

for (i in seq_len(gridsize)) {
  
}