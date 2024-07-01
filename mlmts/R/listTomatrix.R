

# This is a function to transform a list of vectors of the same length into a matrix

listTomatrix <- function(l){
  n <- length(l)
  s <- length(l[[1]])
  m <- matrix(nrow = n, ncol = s)
  for (i in 1 : n) {
    m[i,] <- l[[i]]
  }
  m
}
