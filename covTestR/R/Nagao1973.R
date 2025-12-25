#' @export
#'
#' @references Nagao, H. (1973). On Some Test Criteria for Covariance 
#' Matrix. The Annals of Statistics, 1(4), 700-709
#' @rdname structureStatistics
Nagao1973 <- function(x, Sigma = "identity", ...){
  UseMethod("Nagao1973")
}

#' @export
#' @keywords internal
#' @importFrom stats pchisq
#'
Nagao1973.covariance <- function(x, Sigma = "identity", ...){
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

  statistic <- Nagao1973_(n, p, S_)
  names(statistic) <- "Chi Squared"

  parameter <- p * (p + 1) / 2
  names(parameter) <- "df"

  null.value <- 0
  names(null.value) <- "difference between the Sample Covariance Matrix and the Null Covariance Matrix Structure"

  p.value <- 1 - pchisq(statistic, parameter)

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
              method = "Nagao 1973 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @export
#' @keywords internal
#' @importFrom stats cov
#' @importFrom stats pchisq
#'
Nagao1973.matrix <- function(x, Sigma = "identity", ...){
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

  statistic <- Nagao1973_(n, p, S_)
  names(statistic) <- "Chi Squared"

  parameter <- p * (p + 1) / 2
  names(parameter) <- "df"

  null.value <- 0
  names(null.value) <- "difference between the Sample Covariance Matrix and the Null Covariance Matrix Structure"
  
  p.value <- 1 - pchisq(statistic, parameter)

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
              method = "Nagao 1973 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @keywords internal
Nagao1973_ <- function(n, p, S_){
  mid <- S_ - diag(1, p)
  n * p * (tr(mid %*% mid) / p) / 2
}
