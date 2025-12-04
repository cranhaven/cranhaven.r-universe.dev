
# Simulated draws from posterior of a Poisson likelihood with gamma prior
# =======================================================================

# The gamma prior is specified by mode and SD.

# y is total count, n is the sample size, eg, y=10 ticks on n=6 rats.

Bpoisson <- function(y, n, priors=NULL, draws=10000, ...) {

  if(!is.null(list(...)$sample)) {
    message("*The 'sample' argument is deprecated, please use 'draws'.*")
    draws <- list(...)$sample
  }
  if(!is.null(priors$mode) && priors$mode <= 0)
    stop("priors$mode must be greater than 0.")
  if(!is.null(priors$sd) && (priors$sd <= 0 ))
    stop("priors$sd must be greater than 0.")

  if(is.null(priors$mode) || is.null(priors$sd)) {
    pr_shape <- 1
    pr_rate <- 0
  } else {
    pr_rate <- with(priors, mode + sqrt(mode^2 + 4 * sd^2) / (2 * sd^2) )
    pr_shape <- 1 + priors$mode * pr_rate
  }

  po_shape <- pr_shape + y
  po_rate <- pr_rate + n

  post <- rgamma(draws, po_shape, po_rate)

  out <- mcmcOutput(data.frame(lambda = post),
      header = "Values drawn from gamma posterior distribution")
  attr(out, "call") <- match.call()
  return(out)
}
