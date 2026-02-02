#' Test the equality of two covariance matrices in population
#'
#' The first step of testing measurement invariance (MI) in multiple-group SEM analysis. The null hypothesis is tested using the method of Lagrange multipliers
#'
#' @param ... The same arguments as for any lavaan model. See \code{lavaan::sem} for more information.
#' @param lamb0 initial coefficients of Lagrange multiplier. If not pre-specified, 0.01 will be used.
#' @return The likelihood ratio statistic, degrees of freedom, and p-value of the test.
#' @details The \code{eqMI.covtest} function is the first step to test MI. Under null hypothesis testing (NHT), a non-significant statistic is generally an overall endorsement of MI. If the null hypothesis is rejected then one may proceed to test other aspects of MI.
#' @references Yuan, K. H., & Chan, W. (2016). Measurement invariance via multigroup SEM: Issues and solutions with chi-square-difference tests. Psychological methods, 21(3), 405-426.
#' @references Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/.
#' @importFrom lavaan lav_matrix_vech
#' @importFrom lavaan lav_matrix_duplication
#' @importFrom lavaan getCov
#' @importFrom stats pchisq
#' @importFrom stats cov
#' @export
#' @examples
#' data(HolzingerSwineford)
#' semmodel<-'
#' L1 =~ V1 + V2 + V3
#' L2 =~ V4 + V5 + V6
#' L3 =~ V7 + V8
#' L4 =~ V9 + V10 + V11
#' '
#' cov.test <- eqMI.covtest(model = semmodel,
#'                          data = HolzingerSwineford,
#'                          group="school")
#'
eqMI.covtest <- function(..., lamb0=NULL){
  dotdotdot <- list(...)

  #obtain sample covariance
  if(is.null(dotdotdot$sample.cov) & is.null(dotdotdot$data))
    stop("sample covariances must be provided")
  if (!is.null(dotdotdot$sample.cov)){
    sample.cov <- dotdotdot$sample.cov
    sample.nobs <- dotdotdot$sample.nobs
  } else {
    group.ind <- match(dotdotdot$group, colnames(dotdotdot$data))
    dat.by.group <- split(dotdotdot$data[,-group.ind], f = dotdotdot$data[,group.ind])
    sample.cov <- lapply(dat.by.group, cov)
    sample.nobs <- sapply(dat.by.group, nrow)
  }
  #no missing data or NA in sample.cov
  if(sum(is.na(sample.cov[[1]])+is.na(sample.cov[[2]]))>1)
    stop("Check missing data in raw data or NAs in the sample covariances")
  if (length(sample.nobs)!=2)
    stop("projection method only applies to two groups with this function")

  scov_1 <- sample.cov[[1]]
  scov_2 <- sample.cov[[2]]
  n_1 <- sample.nobs[[1]]
  n_2 <- sample.nobs[[2]]
  p <- nrow(sample.cov[[1]])
  N <- n_1 + n_2
  r_1 <- (n_1-1)/(N-2)
  r_2 <- (n_2-1)/(N-2)
  ep <- .00001
  ps <- p*(p+1)/2
  theta0 <- rep(rowMeans(sapply(sample.cov, lav_matrix_vech)), 2)
  q <- length(theta0)
  df0 <- 2*ps-q

  #LM test coefficients
  if(is.null(lamb0)){
    lamb0 <- matrix(0.01,ps,1)
  }
  theta_lamb0 <- c(theta0, lamb0)

  dup <- lav_matrix_duplication(p)       #duplication matrix

  dtheta <- 1
  n_div <- 1
  for (jj in 1:300){
    theta0 <- theta_lamb0[1:q]
    theta_01 <- theta0[1:ps]
    theta_02 <- theta0[(ps+1):(2*ps)]
    lamb0 <- as.matrix(theta_lamb0[(2*ps+1):(3*ps)])

    sigma_01 <- getCov(theta_01, lower = F)
    sigma_02 <- getCov(theta_02, lower = F)

    sig_in1 <- solve(sigma_01)
    sig_in2 <- solve(sigma_02)

    ## weight given by normal theory
    weight_1 <- 0.5*t(dup)%*%(sig_in1%x%sig_in1)%*%dup
    weight_2 <- 0.5*t(dup)%*%(sig_in2%x%sig_in2)%*%dup

    dw_1 <- weight_1
    dw_2 <- weight_2

    vres_1 <- lav_matrix_vech(scov_1)-theta_01
    vres_2 <- lav_matrix_vech(scov_2)-theta_02

    dfML1 <- dw_1%*%vres_1
    dfML2 <- dw_2%*%vres_2
    dfML <- c(r_1*dfML1, r_2*dfML2)

    dwd_1 <- dw_1
    dwd_2 <- dw_2
    ddf <- rbind(cbind(r_1*dwd_1, matrix(0, ps,ps)), cbind(matrix(0, ps,ps), r_2*dwd_2))

    h1 <- theta_02 - theta_01
    dh1 <- cbind(diag(-1, ps), diag(1, ps))
    dh1_t <- t(dh1)
    dfML_a <- as.matrix(c(-dfML+dh1_t%*%lamb0, h1))

    Amat <- rbind(cbind(ddf, dh1_t), cbind(dh1, matrix(0, ps,ps)))
    Amat_in <- solve(Amat)
    dtheta <- Amat_in%*%dfML_a

    theta_lamb0 <- theta_lamb0-dtheta
    if(max(abs(dtheta))>ep) break

  }

  htheta <- theta_lamb0[1:q]
  n_div <- 0
  Ssigin_1 <- scov_1%*%sig_in1
  Ssigin_2 <- scov_2%*%sig_in2

  fml_1 <- sum(diag(Ssigin_1))-log(det(Ssigin_1))-p
  fml_2 <- sum(diag(Ssigin_2))-log(det(Ssigin_2))-p

  F_ML <- r_1*fml_1+r_2*fml_2
  T_ML <- (N-2)*F_ML
  df <- df0+ps
  pv <- 1-pchisq(T_ML, df)

  cov.test <- cbind(Chisq=T_ML, Df=df, pvalue=pv)
  rownames(cov.test) <- 'fit.pop.cov'
  return(cov.test)
}

