#' @export
#'
#' @references Ledoit, O., and Wolf, M. (2002). Some Hypothesis Tests 
#' for the Covariance Matrix When the Dimension Is Large Compared to 
#' the Sample Size. The Annals of Statistics, 30(4), 1081-1102. 
#' \href{http://doi.org/10.1214/aos/1031689018}{10.1214/aos/1031689018}
#' @rdname structureStatistics
LedoitWolf2002 <- function(x, Sigma = "identity", ...){
  UseMethod("LedoitWolf2002")
}

#' @export
#' @keywords internal
#' @importFrom stats pchisq
#'
LedoitWolf2002.covariance <- function(x, Sigma = "identity", ...){
  p <- ncol(x)
  n <- attributes(x)$df + 1
  S <- x

  if(Sigma[[1]] == "identity"){
    S_ <- x
  }else{
    svCov <- svd(x)
    sv <- svd(Sigma)
    x_ <- svCov$u %*% diag(sqrt(svCov$d)) %*%
      solve(sv$u %*% diag(sqrt(sv$d)))
    S_ <- t(x_) %*% x_
  }

  statistic <- LedoitWolf2002_(n, p, S_)
  names(statistic) <- "Chi Squared"

  parameter <- p * (p + 1) / 2
  names(parameter) <- "df"

  null.value <- 0
  names(null.value) <- "difference between the Sample Covariance Matrix and the Null Covariance Matrix Structure"

  p.value <- 1 - pchisq(statistic, parameter)

  estimate <- S

  obj <- list(statistic = statistic,
              parameter = parameter,
              p.value = p.value,
              estimate = estimate,
              null.value = null.value,
              alternative = "two.sided",
              method = "Ledoit and Wolf 2002 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @export
#' @keywords internal
#' @importFrom stats cov
#' @importFrom stats pchisq
#'
LedoitWolf2002.matrix <- function(x, Sigma = "identity", ...){
  p <- ncol(x)
  n <- nrow(x)
  S <- cov(x)

  if(Sigma[[1]] == "identity"){
    S_ <- S
  }else{
    sv <- svd(Sigma)
    svDf <- svd(S)
    x_ <- svDf$u %*% diag(sqrt(sv$d)) %*% solve(sv$u %*% diag(sqrt(sv$d)))
    S_ <- t(x_) %*% x_
  }

  statistic <- LedoitWolf2002_(n, p, S_)
  names(statistic) <- "Chi Squared"

  parameter <- p * (p + 1) / 2
  names(parameter) <- "df"

  null.value <- 0
  names(null.value) <- "difference between the Sample Covariance Matrix and the Null Covariance Matrix Structure"
  
  p.value <- 1 - pchisq(statistic, parameter)

  estimate <- S

  obj <- list(statistic = statistic,
              parameter = parameter,
              p.value = p.value,
              estimate = estimate,
              null.value = null.value,
              alternative = "two.sided",
              method = "Ledoit and Wolf 2002 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @keywords internal
LedoitWolf2002_ <- function(n, p, S_){
  mid <- S_ - diag(1, p)
  n * p * (tr(mid %*% mid) / p -
    (p / n) * ((tr(S_) / p) ^ 2) +
    (p / n)) / 2
}
