nice_nrow_ncol <- function(n, increasing = TRUE, maxsteps = 20, maxratio = 3) {
  # Proposes a suitable number of columns and rows for the total number of plots.
  #  | 1 | 2 |...|ncol|
  #  | 2 |   |   |    |
  #  |...|   |   |    |
  #  |nrow|  |   |    |
  #
  #  The total number of cells should be higher than the number of plots.

  sq <- sqrt(n)
  sqlow <- floor(sq)
  y <- x <- squpp <- ceiling(sq)
  remainder <- lower <- upper <- c()

  for (i in 1:maxsteps) {
    while (y*x >= n){
      y <- y-1
    }
    lower <- c(lower, y+1)
    upper <- c(upper, x)
    remainder <- c(remainder, (y+1)*x-n)
    x <- x+1
    if ((y < 1) | (x/y > maxratio))
        break
  }
  
  # find the first pair [x, y] minimizing the remainder
  i <- which.min(remainder)
  if (increasing){
    nrow <- lower[i]
    ncol <- upper[i]
  } else {
    nrow <- upper[i]
    ncol <- lower[i]
  }
  
  return(c(nrow, ncol))
}
