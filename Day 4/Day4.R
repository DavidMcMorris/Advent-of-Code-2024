library(dplyr)

input_filename <- "sample.txt"
input <- read.table(input_filename)[[1]]
gridsize <- nchar(input[1])

# Verticals
num_xmas_h <- sum(gregexpr("XMAS", input) != -1)
num_samx_h <- sum(gregexpr("SAMX", input) != -1)

# Horizontals
input_v <- input %>%
  strsplit(., "") %>%
  unlist() %>%
  matrix(., nrow = gridsize) %>%
  apply(., 1, paste0, collapse = "")

num_xmas_v <- sum(gregexpr("XMAS", input_v) != -1)
num_samx_v <- sum(gregexpr("SAMX", input_v) != -1)

print(num_xmas_h + num_samx_h + num_xmas_v + num_samx_v)