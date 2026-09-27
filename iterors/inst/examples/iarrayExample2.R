library(itertools)

display <- function(it, level=0) {
  it <- iteror(it)
  i <- 0
  repeat {
    y <- nextOr(it, break)
    if (i > 0) cat(', ')
    if (inherits(y, 'iter')) {
      cat('{')
      display(y, level=level+1)
      cat('}')
    } else {
      if (length(y) > 1)
        cat(sprintf('[%s]', paste(y, collapse=', ')))
      else
        cat(sprintf('%s', y))
    }
    i <- i + 1
  }
  if (level == 0) cat('\n')
}

x <- array(seq_len(2*3*2), c(2,3,2))
display(iarray(x, c(1,2,3)))
display(iarray(x, c(3,2,1)))
display(iarray(x, c(2,3)))
display(iarray(x, c(3,2)))
display(iarray(x[,,1], 2))
display(iarray(x[,,2], 1))
