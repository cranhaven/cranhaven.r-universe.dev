#' @title strsplit2
#' @description strsplit2
#' @param x character vector
#' @param split character
#' @return matrix
#' @export
strsplit2 <- function(x, split) {
  x <- as.character(x)
  n <- length(x)
  s <- strsplit(x, split = split)
  nc <- unlist(lapply(s, length))
  out <- matrix("", n, max(nc))
  for (i in 1:n) {
    if (nc[i])
      out[i, 1:nc[i]] <- s[[i]]
  }
  out
}
