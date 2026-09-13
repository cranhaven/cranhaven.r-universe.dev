#'  chisquare.test
#'
#' This function does the chisquare test 
#' @param x data set
#' @param  case setup info
#' @param  which type of binning (either RGd, Equal Size or Equal Prob)
#' @return A numeric vector of length 1 with the value of the chi-square statistic.
#' @export
#' @examples
#' case <- list(B=1000, param = NULL, n = 1000, pnull = function(x, param) punif(x), 
#'         rnull = function(n, param) runif(n), qnull = function(x, param) qunif(x), 
#'         est.mle = function(x) NA, nbins = 10)
#' x <- runif(1000)
#' chisquare.test(x, case)               

chisquare.test <- function (x, case, which="RGd") {
  bin.fun <- function (case, k, kappa) {
    n <- case$n
    L <- min(case$dta)
    R <- max(case$dta)
    bins0 <- c(L, case$qnull((1:(k - 1))/k, case$param), R)
    if (k == 2) 
      bins1 <- c(L, case$qnull(0.5, case$param), R)
    else {
      if (is.finite(L) & is.finite(R)) 
        bins1 <- seq(L, R, length = k + 1)
      if (is.finite(L) & !is.finite(R)) {
        R <- case$qnull(1 - 5/n, case$param)
        bins1 <- c(seq(L, R, length = k), Inf)
      }
      if (!is.finite(L) & is.finite(R)) {
        L <- case$qnull(5/n, case$param)
        bins1 <- c(-Inf, seq(L, R, length = k))
      }
      if (!is.finite(L) & !is.finite(R)) {
        L <- case$qnull(5/n, case$param)
        R <- case$qnull(1 - 5/n, case$param)
        bins1 <- c(-Inf, seq(L, R, length = k - 1), Inf)
      }
    }
    bins <- (1 - kappa) * bins0 + kappa * bins1
    if (is.nan(bins[1])) 
      bins[1] <- (-Inf)
    if (is.nan(bins[k + 1])) 
      bins[k + 1] <- Inf
    bins <- bin.adjust(case, bins)
    bins
  }
  bin.adjust <- function (case, bins) {
    p <- case$param
    E <- case$n * diff(case$pnull(bins, p))
    if (all(E > 5)) return(bins)
    nbins <- length(E)
    repeat {
      k <- which.min(E)
      if (k == 1) {
        bins <- bins[-2]
        E[1] <- E[1] + E[2]
        E <- E[-2]
      }
      if (k == nbins) {
        bins <- bins[-nbins]
        E[nbins - 1] <- E[nbins] + E[nbins - 1]
        E <- E[-nbins]
      }
      if (k > 1 & k < nbins) {
        if (E[k - 1] < E[k + 1]) {
          bins <- bins[-k]
          E[k] <- E[k] + E[k - 1]
          E <- E[-k]
        }
        else {
          bins <- bins[-(k + 1)]
          E[k] <- E[k] + E[k + 1]
          E <- E[-(k + 1)]
        }
      }
      nbins <- nbins - 1
      if (all(E > 5)) break
    }
    bins
  }
  if(which=="Equal Prob") {kappa <- 0;k <- ifelse(is.null(case$nbins), 10, case$nbins)}
  if(which=="RGd") {kappa <- 0.5;k <- 5+length(case$param)}
  if(which=="Equal Size") {kappa <- 1; k <- ifelse(is.null(case$nbins), 10, case$nbins)}
  case$dta <- x
  bins <- bin.fun(case, k = k, kappa = kappa)
  tmpbins <- c(-Inf, bins[2:(length(bins)-1)], Inf)
  O <- graphics::hist(x, breaks = tmpbins, plot = FALSE)$counts
  E <- length(x)*diff(case$pnull(tmpbins, case$param))
  sum( (O-E)^2/E )
}
