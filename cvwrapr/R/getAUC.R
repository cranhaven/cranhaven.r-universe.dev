#' @importFrom survival concordance
getAUC <- function(y, prob, weights = rep(1,nrow(y))) {
  Weights <- as.vector(weights * y)
  ny <- nrow(y)
  Y <- rep(c(0, 1), each = ny)
  Prob <- c(prob, prob)

  return(survival::concordance(Y ~ Prob, weights = Weights)$concordance)
}
