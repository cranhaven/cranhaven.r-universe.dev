slategray.colors <- function(n, start = 0.3, end = 0.9) {
  x <- seq(start, end, length.out = n)
  r <- 108 + 49*sapply(x, function(y){min(y, 1/3)})*3 +
      26*(sapply(x, function(y){min(y, 2/3)})-1/3)*3*(x>1/3) +
      13*(sapply(x, function(y){min(y, 1)})-2/3)*3*(x>2/3)
  g <- 123 + 59*sapply(x, function(y){min(y, 1/3)})*3 +
      29*(sapply(x, function(y){min(y, 2/3)})-1/3)*3*(x>1/3) +
      15*(sapply(x, function(y){min(y, 1)})-2/3)*3*(x>2/3)
  b <- 139 + 66*sapply(x, function(y){min(y, 1/3)})*3 +
      33*(sapply(x, function(y){min(y, 2/3)})-1/3)*3*(x>1/3) +
      17*(sapply(x, function(y){min(y, 1)})-2/3)*3*(x>2/3)
  return(rgb(r, g, b, maxColorValue = 255))
}

