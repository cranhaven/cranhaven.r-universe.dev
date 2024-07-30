inference <- function(fidsamples, param, alpha = 0.05){
  out <- numeric(4L)
  names(out) <- c("mean", "median", "lwr", "upr")
  sample <- fidsamples[["Beta"]][[param]]
  weights <- fidsamples[["Weights"]]
  out[1L] <- sum(sample * weights) # mean
  h <- cbind(sample, weights)
  hsort <- h[order(h[,1L]), ]
  hsum <- cumsum(hsort[, 2L])
  ci_u <- min(which(hsum >= 1-alpha/2))
  ci_l <- min(which(hsum >= alpha/2))
  ci_m <- min(which(hsum >= 0.5))
  out[3L] <- hsort[ci_l, 1L] # lower bound
  out[4L] <- hsort[ci_u, 1L] # upper bound
  out[2L] <- hsort[ci_m, 1L] # estimate (median)
  out
}

#' Summary of fiducial samples
#' @description Summary of the fiducial samples.
#'
#' @param fidsamples fiducial samples, the output of \code{\link{gfilogisreg}}
#' @param conf confidence level
#'
#' @return A matrix with summary statistics: means, medians, and confidence
#'   intervals.
#' @export
#'
#' @examples y <- c(0, 0, 1, 1, 1)
#' x <- c(-2, -1, 0, 1, 2)
#' fidsamples <- gfilogisreg(y ~ x, N = 400) # (N=400 is not serious)
#' gfiSummary(fidsamples)
gfiSummary <- function(fidsamples, conf = 0.95){
  sims <- fidsamples[["Beta"]]
  seq_ <- 1L:ncol(sims)
  names(seq_) <- names(sims)
  out <-
    t(vapply(seq_, function(x) inference(fidsamples, x, 1-conf), numeric(4L)))
  attr(out, "confidence level") <- conf
  out
}

#' Fiducial cumulative distribution function
#' @description Fiducial cumulative distribution function of a parameter of
#'   interest.
#'
#' @param parameter a right-sided formula defining the parameter of interest
#' @param fidsamples fiducial samples, the output of \code{\link{gfilogisreg}}
#'
#' @return The fiducial cumulative distribution function of the parameter.
#'
#' @importFrom lazyeval f_eval_rhs
#' @import spatstat
#' @importFrom spatstat.geom ewcdf
#' @export
#'
#' @examples y <- c(
#'   0, 0, 0, 1,
#'   0, 1, 1, 1
#' )
#' group <- gl(2, 4)
#' fidsamples <- gfilogisreg(y ~ 0 + group, N = 500) # (N=500 is not serious)
#' fcdf <- gfiCDF(~ exp(group1) / exp(group2), fidsamples)
#' fcdf(1)
#' plot(fcdf)
gfiCDF <- function(parameter, fidsamples){
  dataName <- "Beta"
  data <- fidsamples[[dataName]]
  fsims <- f_eval_rhs(parameter, data = data)
  ewcdf(fsims, weights = fidsamples[["Weights"]])
}

#' Fiducial confidence interval
#' @description Fiducial confidence interval of a parameter of interest.
#'
#' @param parameter a right-sided formula defining the parameter of interest
#' @param fidsamples fiducial samples, the output of \code{\link{gfilogisreg}}
#' @param conf confidence level
#'
#' @return The fiducial confidence interval of the parameter.
#'
#' @import spatstat
#' @importFrom spatstat.geom quantile.ewcdf
#' @export
#'
#' @examples y <- c(
#'   0, 0, 0, 1,
#'   0, 1, 1, 1
#' )
#' group <- gl(2, 4)
#' fidsamples <- gfilogisreg(y ~ 0 + group, N = 500) # (N=500 is not serious)
#' expit <- function(x) exp(x) / (1+exp(x))
#' gfiConfInt(~ expit(group1) - expit(group2), fidsamples)
gfiConfInt <- function(parameter, fidsamples, conf = 0.95){
  fcdf <- gfiCDF(parameter, fidsamples)
  alpha <- 1 - conf
  quantile.ewcdf(fcdf, c(alpha/2, 1-alpha/2))
}

#' Fiducial quantiles
#' @description Quantiles of the fiducial distribution of a parameter of
#'   interest.
#'
#' @param parameter a right-sided formula defining the parameter of interest
#' @param fidsamples fiducial samples, the output of \code{\link{gfilogisreg}}
#' @param probs numeric vector of probabilities
#'
#' @return Numeric vector of quantiles, of the same length as \code{probs}.
#'
#' @import spatstat
#' @importFrom spatstat.geom quantile.ewcdf
#' @export
#'
#' @examples y <- c(
#'   0, 0, 0, 1,
#'   0, 1, 1, 1
#' )
#' group <- gl(2, 4)
#' fidsamples <- gfilogisreg(y ~ 0 + group, N = 500) # (N=500 is not serious)
#' gfiQuantile(~ group2 - group1, fidsamples, c(25, 50, 75)/100)
gfiQuantile <- function(parameter, fidsamples, probs){
  fcdf <- gfiCDF(parameter, fidsamples)
  quantile.ewcdf(fcdf, probs = probs)
}
