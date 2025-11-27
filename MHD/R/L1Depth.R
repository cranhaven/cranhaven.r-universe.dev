LpAve <- function(dataX, p=1) {
  if (p > 1) {
    colMeans(apply(dataX, 2, `^`, p=p))^(1/p)
  } else if (p == 1) {
    colMeans(dataX)
  }
}


