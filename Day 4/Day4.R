library(dplyr)

input_filename <- "sample.txt"
input <- read.table(input_filename)[[1]]
gridsize <- nchar(input[1])

# Horizontals
num_xmas_h <- sum(sapply(gregexpr("XMAS", input), function(x) sum(x != -1)))
num_samx_h <- sum(sapply(gregexpr("SAMX", input), function(x) sum(x != -1)))

# Vericals
input_v <- input %>%
  strsplit(., "") %>%
  unlist() %>%
  matrix(., nrow = gridsize) %>%
  apply(., 1, paste0, collapse = "")

num_xmas_v <- sum(sapply(gregexpr("XMAS", input_v), function(x) sum(x != -1)))
num_samx_v <- sum(sapply(gregexpr("SAMX", input_v), function(x) sum(x != -1)))

print(num_xmas_h + num_samx_h + num_xmas_v + num_samx_v)

# Split strings for diagonal search
# input <- input %>%
#   strsplit(., "") %>%
#   unlist() %>%
#   matrix(., nrow = gridsize, byrow = TRUE)

# x_inds <- which(input == "X")

# diag_search <- function(ind) {

# }