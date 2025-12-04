
# Simulated draws from posterior of a binomial likelihood with beta prior
# =======================================================================

# The beta prior is specified by mode and concentration.

# Bbinom is retained for backward compatibility but will shortly be deprecated.

Bbinomial <- function(y, n, priors=NULL, draws=100000, ...) {

  if(!is.null(list(...)$sample)) {
    message("*The 'sample' argument is deprecated, please use 'draws'.*")
    draws <- list(...)$sample
  }

  if(!is.null(priors$conc) && priors$conc < 2)
    stop("priors$conc must not be less than 2.")
  if(!is.null(priors$mode) && (priors$mode < 0 || priors$mode > 1 ))
    stop("priors$mode must be between 0 and 1.")
  if(y > n)
    stop("Number of successes (y) cannot be greater than the number of trials (n).")

  if(!is.null(priors$conc) && !is.null(priors$mode)) {
    pr1 <- priors$mode * (priors$conc - 2) + 1
    pr2 <- (1 - priors$mode) * (priors$conc - 2) + 1
  } else {
    pr1 <- pr2 <- 1
  }

  po1 <- pr1 + y
  po2 <- pr2 + n - y

  post <- rbeta(draws, po1, po2)

  out <- mcmcOutput(data.frame(p = post),
      header = "Values drawn from beta posterior distribution")
  attr(out, "call") <- match.call()
  return(out)
}
# ..............................................................

Bbinom <- function(...) {
  warning("[Bbinom] is deprecated; please use [Bbinomial] instead.", call. = FALSE)
  Bbinomial(...)
}
