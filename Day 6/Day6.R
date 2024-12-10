library(dplyr)

input_filename <- "input.txt"
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

walk <- function(guard, direction, obstructions) {
  if (direction == 1) {
    tryCatch(
      {
        next_obs <- obstructions %>%
          filter(X2 == guard[[2]], X1 < guard[[1]]) %>%
          filter(X1 == max(X1))
        new_guard <- t(matrix(as.vector(next_obs + c(1, 0))))
        direction <- 2
        flag <- 0
        # locs <- new_guard[[1]]:guard[[1]]
        # loc_hist <- matrix(c(locs, rep(guard[[2]], length(locs))), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))
      }, error = function(cond) {
        flag <- 1
        new_guard <- rbind(c(1, guard[[2]]))
        # locs <- new_guard[[1]]:guard[[1]]
        # loc_hist <- matrix(c(locs, rep(guard[[2]], length(locs))), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))
      }, warning = function(cond) {
        flag <- 1
        new_guard <- rbind(c(1, guard[[2]]))
        # locs <- new_guard[[1]]:guard[[1]]
        # loc_hist <- matrix(c(locs, rep(guard[[2]], length(locs))), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))
      }
    )
  } else if (direction == 2) {
    tryCatch(
      {
        next_obs <- obstructions %>%
          filter(X1 == guard[[1]], X2 > guard[[2]]) %>%
          filter(X2 == min(X2))
        new_guard <- t(matrix(as.vector(next_obs + c(0, -1))))
        direction <- 3
        flag <- 0
        # locs <- guard[[2]]:new_guard[[2]]
        # loc_hist <- matrix(c(rep(guard[[1]], length(locs)), locs), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))
      }, error = function(cond) {
        flag <- 1
        new_guard <- rbind(c(guard[[1]], gridsize))
        # locs <- guard[[2]]:new_guard[[2]]
        # loc_hist <- matrix(c(rep(guard[[1]], length(locs)), locs), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))
      }, warning = function(cond) {
        flag <- 1
        new_guard <- rbind(c(guard[[1]], gridsize))
        # locs <- guard[[2]]:new_guard[[2]]
        # loc_hist <- matrix(c(rep(guard[[1]], length(locs)), locs), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))
      }
    )
  } else if (direction == 3) {
    tryCatch(
      {
        next_obs <- obstructions %>%
          filter(X2 == guard[[2]], X1 > guard[[1]]) %>%
          filter(X1 == min(X1))
        new_guard <- t(matrix(as.vector(next_obs + c(-1, 0))))
        direction <- 4
        flag <- 0
        # locs <- guard[[1]]:new_guard[[1]]
        # loc_hist <- matrix(c(locs, rep(guard[[2]], length(locs))), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))
      }, error = function(cond) {
        flag <- 1
        new_guard <- rbind(c(gridsize, guard[[2]]))
        # locs <- guard[[1]]:new_guard[[1]]
        # loc_hist <- matrix(c(locs, rep(guard[[2]], length(locs))), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))
      }, warning = function(cond) {
        flag <- 1
        new_guard <- rbind(c(gridsize, guard[[2]]))
        # locs <- guard[[1]]:new_guard[[1]]
        # loc_hist <- matrix(c(locs, rep(guard[[2]], length(locs))), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))        
      }
    )
  } else if (direction == 4) {
    tryCatch(
      {
        next_obs <- obstructions %>%
          filter(X1 == guard[[1]], X2 < guard[[2]]) %>%
          filter(X2 == max(X2))
        new_guard <- t(matrix(as.vector(next_obs + c(0, 1))))
        direction <- 1
        flag <- 0
        # locs <- new_guard[[2]]:guard[[2]]
        # loc_hist <- matrix(c(rep(guard[[1]], length(locs)), locs), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))        
      }, error = function(cond) {
        flag <- 1
        new_guard <- rbind(c(guard[[1]], 1))
        # locs <- new_guard[[2]]:guard[[2]]
        # loc_hist <- matrix(c(rep(guard[[1]], length(locs)), locs), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))        
      }, warning = function(cond) {
        flag <- 1
        new_guard <- rbind(c(guard[[1]], 1))
        # locs <- new_guard[[2]]:guard[[2]]
        # loc_hist <- matrix(c(rep(guard[[1]], length(locs)), locs), ncol = 2, byrow = FALSE)
        loc_hist <- NULL
        return(list(guard = new_guard, direction = direction, flag = flag, loc_hist = loc_hist))        
      }
    )
  }
}

# Uncomment for part 1
# loc_info <- list(guard = guard, direction = direction, flag = 0)
# loc_hist <- paste0(guard, collapse = ",")
# while (loc_info$flag == 0) {
#   loc_info <- walk(loc_info$guard, loc_info$direction, obstructions)
#   loc_hist <- c(loc_hist, apply(loc_info$loc_hist, 1, paste0, collapse = ","))
# }

# print(length(unique(loc_hist)))

# Part 2
loops <- 0

l <- length(input)
for (i in seq_along(input)) {
  new_input <- input
  arr_ind <- arrayInd(i, dims)
  if (new_input[i] != "O" && !identical(arr_ind, t(guard))) {
    new_input[i] <- "O"
    new_obstructions <- rbind(obstructions, data.frame(arr_ind))
    flag <- 0
    loc_info <- list(guard = guard, direction = direction, flag = flag)
    loc_hist <- paste0(c(loc_info$guard, loc_info$direction), collapse = ",")
    while (loc_info$flag == 0) {
      loc_info <- walk(loc_info[[1]], loc_info[[2]], new_obstructions)
      if (paste0(c(loc_info$guard, loc_info$direction), collapse = ",") %in% loc_hist) {
        loc_info$flag <- 2
        loops <- loops + 1
      } else {
        loc_hist <- c(loc_hist, paste0(c(loc_info$guard, loc_info$direction), collapse = ","))
      }
    }
  }
}

print(loops)