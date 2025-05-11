# This function gets the (vector form) of the covariance specification from an
# lm or nls object.
get_cov <- function(m, verbose){
  stopifnot(is(m, "lm") || is(m, "nls"))
  m_class <- if(is(m, "lm")) "lm" else "nls"
  w <- weights(m)
  if(is.null(w)){
    if(isTRUE(verbose)) message("Using residual standard deviation from ", m_class, " object as covariance specification...")
    covariance <- list(SqrtSigma = sigma(m))
  } else {
    if(isTRUE(verbose)) message("Using weights from ", m_class, " object as covariance specification...")
    covariance <- list(P = w)
  }
}
