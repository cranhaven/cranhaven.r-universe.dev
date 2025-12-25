#' @export
#'
#' @references Ahmad, M. R. and Rosen, D. von. (2015). Tests for 
#' High-Dimensional Covariance Matrices Using the Theory of U-statistics. 
#' Journal of Statistical Computation and Simulation, 85(13), 2619-2631. 
#' \href{http://doi.org/10.1080/00949655.2014.948441}{10.1080/00949655.2014.948441}
#' @rdname structureStatistics
Ahmad2015 <- function(x, Sigma = "identity", ...){
  UseMethod("Ahmad2015")
}

#' @export
#' @keywords internal
#' @importFrom stats pnorm
#'
Ahmad2015.covariance <- function(x, Sigma = "identity", ...){
  p <- ncol(x)
  n <- attributes(x)$df + 1
  S <- x

  if(Sigma[[1]] == "identity"){
    svCov <- svd(x)
    x_ <- svCov$u %*% diag(sqrt(svCov$d))
  }else{
    svCov <- svd(x)
    sv <- svd(Sigma)
    x_ <- svCov$u %*% diag(sqrt(svCov$d)) %*%
      solve(sv$u %*% diag(sqrt(sv$d)))
  }


  statistic <- Ahmad2015Stat(x_)
  names(statistic) <- "Normal"

  parameter <- c(0, 4 * (2 / (p / n + 1)))
  names(parameter) <- c("Mean", "Variance")

  null.value <- 0
  names(null.value) <- "difference between the Sample Covariance Matrix and the Null Covariance Matrix Structure"

  p.value <- 1 - pnorm(abs(statistic), 0, 4 * (2 / (p / n + 1)))

  estimate <- S
  estimate <- if(nrow(estimate) > 5){
    NULL
  }else{
    estimate
  }

  obj <- list(statistic = statistic,
              parameter = parameter,
              p.value = p.value,
              estimate = estimate,
              null.value = null.value,
              alternative = "two.sided",
              method = "Ahmad and Rosen 2015 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @export
#' @keywords internal
#' @importFrom stats cov
#' @importFrom stats pnorm
#'
Ahmad2015.matrix <- function(x, Sigma = "identity", ...){
  p <- ncol(x)
  n <- nrow(x)
  S <- cov(x)

  if(Sigma[[1]] == "identity"){
    x_ <- x
  }else{
    sv <- svd(Sigma)
    x_ <- x %*% solve(sv$u %*% diag(sqrt(sv$d)))
  }

  statistic <- Ahmad2015Stat(x_)
  names(statistic) <- "Normal"

  parameter <- c(0, 4 * (2 / (p / n + 1)))
  names(parameter) <- c("Mean", "Variance")

  null.value <- 0
  names(null.value) <- "difference between the Sample Covariance Matrix and the Null Covariance Matrix Structure"

  p.value <- 1 - pnorm(abs(statistic), 0, 4 * (2 / (p / n + 1)))

  estimate <- S
  estimate <- if(nrow(estimate) > 5){
    NULL
  }else{
    estimate
  }

  obj <- list(statistic = statistic,
              parameter = parameter,
              p.value = p.value,
              estimate = estimate,
              null.value = null.value,
              alternative = "two.sided",
              method = "Ahmad and Rosen 2015 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}
