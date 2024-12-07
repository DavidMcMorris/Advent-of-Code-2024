library(dplyr)

input_filename <- "sample.txt"
input <- read.table(input_filename)[[1]]
gridsize <- length(input)

input <- input %>%
  strsplit(., "") %>%
  unlist() %>%
  matrix(., nrow = gridsize, byrow = TRUE)

dims <- dim(input)

obstructions <- data.frame(arrayInd(which(input == "O"), dims))
direction <- 1
guard <- arrayInd(which(input == "^"), dims)
# turn_mat <- matrix(c(0, -1, 1, 0), nrow = 2)

coord_tester <- function(mat) {
  sum(mat <= 0) == 0 && sum(mat > dims[1]) == 0
}

# walk <- function(guard, direction, obstructions) {
#   next_step <- guard + direction
#   if (paste0(next_step, collapse = ",") %in% apply(obstructions, 1, paste0, collapse = ",")) {
#     direction <- turn_mat %*% direction
#   } else {
#     guard <- next_step
#   }
#   return(list(guard, direction))
# }

walk <- function(guard, direction, obstructions) {
  if (direction == 1) {
    tryCatch(
      {
        next_obs <- obstructions %>% 
          filter(X2 == guard[2], X1 < guard[1]) %>%
          filter(X1 == max(X1))
        guard <- next_obs + c(1, 0)
        direction <- (direction + 1) %% 4
        flag <- 0
        return(list(guard = guard, direction = direction, flag = flag))
      }, error = function(cond) {
        flag <- 1
        return(list(guard = guard, direction = direction, flag = flag))
      }, warning = function(cond) {
        flag <- 1
        return(list(guard = guard, direction = direction, flag = flag))
      }
    )
  }
}

flag <- 0
loc_info <- list(guard, direction)
loc_hist <- paste0(guard, collapse = ",")
while (flag == 0) {
  loc_info <- walk(loc_info[[1]], loc_info[[2]], obstructions)
  if (!coord_tester(loc_info[[1]])) {
    flag <- 1
  } else {
    loc_hist <- c(loc_hist, paste0(loc_info[[1]], collapse = ","))
  }
}

print(length(unique(loc_hist)))

# Part 2
# loops <- 0

# for (i in seq_along(input)) {
#   new_input <- input
#   arr_ind <- arrayInd(i, dims)
#   if (new_input[i] != "O" && !identical(arr_ind, t(guard))) {
#     new_input[i] <- "O"
#     new_obstructions <- rbind(obstructions, arr_ind)
#     flag <- 0
#     loc_info <- list(guard, direction)
#     loc_hist <- paste0(loc_info, collapse = ",")
#     while (flag == 0) {
#       loc_info <- walk(loc_info[[1]], loc_info[[2]], new_obstructions)
#       if (!coord_tester(loc_info[[1]])) {
#         flag <- 1
#       } else if (paste0(loc_info, collapse = ",") %in% loc_hist) {
#         flag <- 2
#         loops <- loops + 1
#       } else {
#         loc_hist <- c(loc_hist, paste0(loc_info, collapse = ","))
#       }
#     }
#   }
# }

# print(loops)