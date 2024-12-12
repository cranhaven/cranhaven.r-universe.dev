#' Dexpectilize a vector according the a single asymmetric point
#' @description This function is part of the erfe package. It estimates
#'  the ERFE model for a panel dataset and for a
#'  single asymmetric point \eqn{\tau \in (0, 1)}.
#'  When \eqn{\tau=0.5} the function estimate the classical
#'  within-transformation estimator and its sandwich
#'  covariance matrix. 
#' @return Return a list of objects related to the erfe model
#' such as the asymmetric point, the coefficient-estimate,
#' the standard deviation, the estimated covariance.
#' @author Amadou Barry, \email{barryhafia@@gmail.com}
#' @references Barry, Amadou, Oualkacha, Karim, and Charpentier 
#'  Arthur. (2022). \emph{Weighted asymmetric least squares
#'   regression with fixed-effects}.
#'  arXiv preprint arXiv:2108.04737
#' @param xmat Numeric vector to de-expectilize.
#' @param yvec Numeric vector of individual asymmetric weight.
#' @param panSizeVec Numeric vector to individual panel size.
#' @param asym Numeric vector to individual panel size.
#' @param id Numeric vector to individual panel size.
#' @examples
#' set.seed(13)
#' temps_obs <- 5
#' n_subj <- 50
#' sig <- diag(rep(1,temps_obs))
#' id <- rep(1:n_subj, each=temps_obs)
#' rvec <- c(mvtnorm::rmvnorm(n_subj, sigma = sig))
#' fvec <- (1 + rep(rnorm(n_subj) , each=temps_obs))
#' xmat <- cbind(rt(n_subj*temps_obs, df=2, ncp=1.3),
#'  1.2 * fvec + rnorm(n_subj * temps_obs, mean = 0.85, sd = 1.5) )
#' yvec <- 0.6*xmat[, 1] + xmat[, 2] + fvec + rvec
#' asym <- 0.5
#' panSizeVec <- unname(unlist(lapply(split(id, id), function(x) length(x))))
#' erfeVecR(xmat, yvec, panSizeVec, asym, id)
#' @export
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats lsfit
#' @importFrom Matrix bdiag
erfeVecR <- function(xmat, yvec, panSizeVec, asym, id) {
  xncol <- ncol(xmat) 
  betaEstimate <- rep(NA, xncol)
  it <- 1
  dw1 <- 1
  w1 <- 0 * yvec + 0.5
  while ( dw1 != 0 && it < 50 ) {
    ydexpect <- dexpectilizeVecR(yvec, w1, panSizeVec)
    xdexpect <- dexpectilizeMatR(xmat, w1, panSizeVec)
    lsfit_coef <- stats::lsfit(xdexpect, ydexpect, 
                               wt = w1, intercept = FALSE)$coef
    betaEstimate <- lsfit_coef
    z1 <- c(xdexpect %*% betaEstimate)
    w01 <- w1
    w1[] <- asym
    w1[ !(ydexpect > z1) ] <- 1 - asym
    dw1 <- sum(w1 != w01, na.rm = TRUE)
    it <- it + 1
    if (it == 49)
      warning("IWLS weights did not converge 
              after 50 iterations.")
  }
  res <- c(yvec - z1)
  weight_res <- c(w1) * res
  split_id <- split(id, id, drop = TRUE)
  bdiag_sigma <- Matrix::bdiag(lapply(split_id, function(x)
    { tcrossprod(weight_res[which(id==x)]) } ) )
  mat_bread <- solve(t(xmat) %*% diag(c(w1)) %*% xmat)  
  mat_meat <- t(xmat) %*% bdiag_sigma %*% xmat
  denom_dof <- length(weight_res) - ncol(xmat) - length(split_id)
  dof <- length(weight_res) / denom_dof
  cov_mat <- (dof * mat_bread) %*% mat_meat %*% mat_bread
  ecart_type <- sqrt(diag(as.matrix(cov_mat)))
  out <- list(asymPoint = asym,
              coefEst = betaEstimate,
              standardError = ecart_type,
              asymWeight = c(w1),
              Residuals = res,
              covMat = cov_mat)
  out
}
