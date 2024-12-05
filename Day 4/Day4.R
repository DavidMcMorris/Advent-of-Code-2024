library(dplyr)

input_filename <- "input.txt"
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

# Split strings for diagonal search
input <- input %>%
  strsplit(., "") %>%
  unlist() %>%
  matrix(., nrow = gridsize, byrow = TRUE)

x_inds <- arrayInd(which(input == "X"), rep(gridsize, 2))
se <- matrix(rep(0:3, 2), ncol = 2)
sw <- se %*% rbind(c(1, 0), c(0, -1))
ne <- sw[,c(2,1)]
nw <- -se

coord_tester <- function(mat) {
  sum(mat <= 0) == 0 && sum(mat > gridsize) == 0
}

word_tester <- function(word) {
  word == "XMAS" | word == "SAMX"
}

diag_search <- function(ind) {
  count <- 0
  coord_mat <- matrix(rep(ind, 4), ncol = 2, byrow = TRUE)
  se_coords <- coord_mat + se
  sw_coords <- coord_mat + sw
  ne_coords <- coord_mat + ne
  nw_coords <- coord_mat + nw
  coord_list <- list(se_coords, sw_coords, ne_coords, nw_coords)
  for (i in 1:4) {
    if (coord_tester(coord_list[[i]])) {
      word <- paste0(input[coord_list[[i]]], collapse = "")
      if (word_tester(word)) {
        count <- count + 1
      }
    }
  }
  return(count)
}

num_diags <- 0
for (i in seq_len(nrow(x_inds))) {
  num_diags <- num_diags + diag_search(x_inds[i, ])
}

print(num_diags + num_xmas_h + num_samx_h + num_xmas_v + num_samx_v)

# Part 2
pos <- rbind(c(1, -1), c(0, 0), c(-1, 1))
neg <- rbind(c(-1, -1), c(0, 0), c(1, 1))

a_inds <- arrayInd(which(input == "A"), rep(gridsize, 2))

mas_word_tester <- function(word) {
  word == "MAS" | word == "SAM"
}
mas_search <- function(ind) {
  word_flag <- c(0, 0)
  coord_mat <- matrix(rep(ind, 3), ncol = 2, byrow = TRUE)
  pos_coords <- coord_mat + pos
  neg_coords <- coord_mat + neg
  coord_list <- list(pos_coords, neg_coords)
  for (i in 1:2) {
    if (coord_tester(coord_list[[i]])) {
      word <- paste0(input[coord_list[[i]]], collapse = "")
      if (mas_word_tester(word)) {
        word_flag[i] <- 1
      }
    }
  }
  return(prod(word_flag))
}

num_mas <- 0
for (i in seq_len(nrow(a_inds))) {
  num_mas <- num_mas + mas_search(a_inds[i, ])
}

print(num_mas)
