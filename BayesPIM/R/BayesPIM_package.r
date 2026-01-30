#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @useDynLib BayesPIM, .registration = TRUE
#' @importFrom stats dexp dgamma dlnorm dnorm dt dweibull ecdf median optim pexp pgamma plnorm pnorm pweibull qexp qgamma qlnorm qnorm quantile qweibull rbeta rbinom rchisq rexp rgamma rlnorm rmultinom rnorm runif rweibull var
#' @importFrom coda gelman.diag effectiveSize mcmc
#' @importFrom actuar dllogis pllogis qllogis rllogis
#' @import Rcpp coda mvtnorm MASS ggamma doParallel foreach parallel
## usethis namespace: end
NULL