
# Extractor function to get WAIC if objects have a WAIC attribute

WAIC <- function (object, ...) {
  waic1 <- function(x)  {
    wc <- attr(x, "WAIC")
    if(is.null(wc))
      wc <- c(NA, NA)
    return(rev(wc))
  }
  if (length(list(...))) {
    lls <- t(sapply(list(object, ...), waic1))
    Call <- match.call()
    Call$nobs <- NULL
    row.names(lls) <- as.character(Call[-1L])
    return(as.data.frame(lls))
  } else {
    return(waic1(object)[2])
  }
}
