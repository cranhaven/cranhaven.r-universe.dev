# code has been adapted from the ncvreg package (Breheny and Huang, 2011)
standard <- function(X) {
  if (!is(X, "matrix")) {
    tmp <- try(X <- model.matrix(~0+., data=X), silent=TRUE)
    if (is(tmp, "try-error")) stop("X must be a matrix or able to be coerced to a matrix")
  }
  STD <- .Call("standardize", X, PACKAGE = "SSLASSO")
  dimnames(STD[[1]]) <- dimnames(X)
  ns <- which(STD[[3]] > 1e-6)
  if (length(ns) == ncol(X)) {
    val <- STD[[1]]
  } else {
    val <- STD[[1]][, ns, drop=FALSE]
  }
  attr(val, "center") <- STD[[2]]
  attr(val, "scale") <- STD[[3]]
  attr(val, "nonsingular") <- ns
  val
}
