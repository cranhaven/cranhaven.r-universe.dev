#' @title The adaptive shrinkage estimate for generalized estimating equations
#'
#' @description
#' \code{evaluateGEEModel} is used to get a generalized estimating equation of the
#' data by the adaptive shrinkage estimate method.
#'
#' @details
#' evaluateGEEModel fits the current data by generalized estimating equations(GEE)
#' according to the value of the family argument and the corstr argument. We
#' should notice that this is not the ordinary generalized estimating equations.
#' It can determine the variables that have an impact on the response which
#' called effective variables. We use model selection criteria like the QIC
#' criterion to choose the optimal value.
#' @param family A description of the error distribution and link function to be
#'   used in the model. See family for details of \code{\link{family}}
#'   functions.
#' @param corstr A character string specifying the correlation structure. The
#'   following are permitted: "independence", "exchangeable" and "ar1".
#' @param y The response data.
#' @param x A data frame contains the covariate vectors.
#' @param clusterID The id for each subject in the initial samples. Note that
#'   the subjects in the same cluster will have identical id.
#' @param criterion The model selection criteria, one of the 'PWD' or 'QIC'.
#' @param theta The parameters of the adaptive shrinkage estimate.
#' @param gamma The parameters of the adaptive shrinkage estimate.
#' @param leastVar The minimum number of variables.
#' @param mostVar The maximum number of variables.
#' @return a list containing the following components
#' \item{rho}{the correlation coefficient of the clusters}
#' \item{beta}{parameters that we estimate under the current samples}
#' \item{sandwich}{the sandwich information matrix for covariance}
#' \item{nonZeroIdx}{the index of the non zero coefficients}
#' \item{call}{a list containing several matrices including the sandwich matrix}
#'

evaluateGEEModel <- function(family, corstr, y, x, clusterID, criterion = "QIC", theta = 0.75,
                          gamma = 1, leastVar = 3, mostVar = ncol(x)) {
  LINKS <- c("identity", "logit")
  linkv <- match(family$link, LINKS, -1L)
  CORSTRS <- c("independence", "exchangeable", "ar1")
  corstrv <- match(corstr, CORSTRS, -1L)
  call <- geepack::geese.fit(x, y, clusterID,
                             family = family, scale.fix = TRUE, corstr = corstr )

  getLr <- function(x) {
    S <- t(x[, -1]) %*% x[, -1]
    eigenval <- eigen(S, only.values = TRUE)$values
    maxEigen <- eigenval[1]
    minEigen <- eigenval[length(eigenval)]
    Lr <- minEigen^2 / (maxEigen * log(maxEigen))
    Lr
  }

  # Lr <- getLr(x)
  Lr <- nrow(x)
  thresh <- Lr^(1 / 2 - theta) / abs(call$beta)^gamma

  order_ind <- order(thresh)
  val <- 1e+08

  for (n in leastVar:mostVar) {
    nonZeroIdx <- order_ind[1:n]
    df <- length(nonZeroIdx)
    beta_ase <- rep(0, ncol(x))
    beta_ase[nonZeroIdx] <- call$beta[nonZeroIdx]
    if (corstr == "independence") call$alpha = 0
    val_new <- switch(criterion,
                      #"PWD" = PWD(y, x, clusterID, beta_ase, df, call$alpha, linkv, corstrv),
                      "QIC" = QIC(y, x, clusterID, beta_ase, nonZeroIdx, call$alpha, linkv, corstrv))
    if (val_new > val) next
    nonZero <- nonZeroIdx
    val <- val_new
    beta_hat <- beta_ase
  }

  cov_list <- getMH(y, x, clusterID, call$beta, call$alpha, linkv, corstrv)
  return(list(
    rho = call$alpha,
    beta = beta_hat,
    sandwich = cov_list$sandwich,
    nonZeroIdx = nonZero,
    call = cov_list
  ))
}
