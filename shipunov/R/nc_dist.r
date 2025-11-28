NC.dist <- function(data, method="gzip", character=TRUE) {
 method <- match.arg(method, c("gzip", "bzip2", "xz"))
 .NCD <- function(x, y) {
  if(character) {
   x <- as.character(x)
   y <- as.character(y)
  } else {
   x <- as.raw(x)
   y <- as.raw(y)
  }
  lx <- length(memCompress(x, type=method))
  ly <- length(memCompress(y, type=method))
  lxy <- length(memCompress(c(x, y), type=method))
  (lxy - min(lx, ly)) / max(lx, ly)
 }
 if (is.data.frame(data)) data <- as.matrix(data)
 d <- outer(seq_len(nrow(data)), seq_len(nrow(data)), Vectorize(function(i, j) .NCD(data[i, ], data[j, ])))
 dimnames(d) <- list(rownames(data), rownames(data))
 as.dist(d)
}
