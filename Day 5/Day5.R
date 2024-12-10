library(dplyr)

input_filename <- "sample.txt"
input <- readLines(input_filename)
linebreak <- which(input == "")
rules <- input[1:(linebreak - 1)] %>%
  strsplit(., split = "|", fixed = TRUE) %>%
  unlist() %>%
  as.numeric() %>%
  matrix(., ncol = 2, byrow = TRUE) %>%
  data.frame()

pages <- input[(linebreak + 1):length(input)] %>%
  strsplit(., split = ",", fixed = TRUE) %>%
  lapply(., as.numeric)

compare <- function(x1, x2) {
  x1sub <- subset(rules, X1 == x1)
  if (x2 %in% x1sub$X2) {
    return(1)
  } else {
    return(0)
  }
}

page_check <- function(page_order) {
  len <- length(page_order)
  for (i in 1:(len - 1)) {
    for (j in (i + 1):len) {
      flag <- compare(page_order[i], page_order[j])
      if (flag == 0) {
        return(flag)
      }
    }
  }
  return(flag)
}

total <- 0
correct_order_inds <- NULL
for (i in seq_along(pages)) {
  flag <- page_check(pages[[i]])
  if (flag == 1) {
    total <- total + pages[[i]][(length(pages[[i]]) + 1) / 2]
    correct_order_inds <- c(correct_order_inds, i)
  }
}

print(total)

# Part 2

out_of_order <- pages[-correct_order_inds]

# Merge sort functions

merge_lists <- function(left, right) {
  result <- list()
  while (length(left) * length(right) > 0) {
    if (compare(left[[1]], right[[1]])) {
      result <- c(result, left[1])
      left <- left[!(left %in% result)]
    } else {
      result <- c(result, right[1])
      right <- right[!(right %in% result)]
    }
  }
  if (length(left) > 0) {
    result <- c(result, left)
  }
  if (length(right) > 0) {
    result <- c(result, right)
  }
  return(result)
}

merge_sort <- function(m) {
  len <- length(m)
  if (length(m) <= 1) {
    return(m)
  } else {
    split_ind <- floor(len / 2)
    left <- m[1:split_ind]
    right <- m[(split_ind + 1) : len]

    left <- merge_sort(left)
    right <- merge_sort(right)

    return(merge_lists(left, right))
  }
}


out_of_order <- merge_sort(out_of_order)
