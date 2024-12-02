input_filename <- "input.txt"
input <- readLines(input_filename)
input <- unlist(lapply(input, strsplit, split = " "), recursive = FALSE)
input <- lapply(input, as.numeric)

checker <- function(report) {
  differences <- diff(report)
  if (length(unique(sign(differences))) == 1) {
    if (max(abs(differences)) <= 3 && min(abs(differences)) >= 1) {
      return(1)
    } else {
      return(0)
    }
  } else {
    return(0)
  }
}

count <- sum(unlist(lapply(input, checker)))
print(count)

count <- 0
for (i in seq_along(input)) {
  for (j in seq_along(input[[i]])) {
    report <- input[[i]][setdiff(seq_along(input[[i]]), j)]
    dampener <- checker(report)
    if (dampener == 1) {
      count <- count + 1
      break
    }
  }
}

print(count)