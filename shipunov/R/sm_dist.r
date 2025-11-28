SM.dist <- function(data, zeroes=TRUE, cut=FALSE) {
 if (cut) data <- sapply(data, function(.x) if (is.numeric(.x)) {
  .y <- cut(.x, breaks=hist(.x, plot=FALSE)$breaks, include.lowest=TRUE, labels=FALSE)
  .y[.x == 0] <- 0
  .y } else .x)
 if (is.data.frame(data)) data <- as.matrix(data)
 if (is.character(data)) data <- apply(data, 2, function(.x) as.integer(factor(.x)))
 d <- outer(seq_len(nrow(data)), seq_len(nrow(data)),
  if(zeroes) {
   if (anyNA(data)) {
    Vectorize(function(i, j) sum(data[i, ] == data[j, ], na.rm=TRUE)/sum(!is.na(data[i, ] + data[j, ])))
   } else {
    Vectorize(function(i, j) sum(data[i, ] == data[j, ])/ncol(data))
   }
  } else {
    Vectorize(function(i, j) sum(data[i, ] == data[j, ] & data[i, ] != 0 & data[j, ] != 0, na.rm=TRUE)/sum(!is.na(data[i, ] + data[j, ]) & (data[i, ] != 0 | data[j, ] != 0)))
  }
)
 dimnames(d) <- list(rownames(data), rownames(data))
 as.dist(1 - d)
}
