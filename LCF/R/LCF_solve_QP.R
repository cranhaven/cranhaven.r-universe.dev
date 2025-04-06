#' Linear combination fitting solve function
#'
#' Quadratic programming solution function for linear combination fitting (LCF)
#' @param LCF.stds Standards for LCF
#' @param LCF.samp Sample for LCF
#' @keywords normalization, correction
#' @export
#' @importFrom quadprog solve.QP
#' @examples
#' data(stdmix)
#' corr.spec.standards  <- initial_load(specdat[1:4], 
#'   corr.norm = c(-36, -15, 37, 58))
#' corr.spec.samples    <- initial_load(specdat[5:8], 
#'   corr.norm = c(-36, -15, 37, 58))
#' fit.standards <- std_df(sample = corr.spec.samples[[1]], 
#'   all.standards = corr.spec.standards)
#' corr.spec <- bkg_corr(raw.spec = corr.spec.samples[[1]], 
#'   corr.norm = c(-36, -15, 37, 58))
#' ## set fitting range parameters relative to E zero
#' E.zero <- corr.spec.samples[[1]]$data$E0
#' LC.pre <- -14
#' LC.post <- 46
#' ## find ranges that have to be fitted
#' abs.pre <- abs(corr.spec[["energy"]]-(E.zero+LC.pre))
#' abs.post <- abs(corr.spec[["energy"]]-(E.zero+LC.post))
#' range.pre <- which(abs.pre == min(abs.pre))
#' range.post <- which(abs.post == min(abs.post))
#' ## extract standards and sample in given range
#' LC.sample <- corr.spec["cor.absorption"][range.pre:range.post,]
#' LC.standards <- fit.standards[range.pre:range.post,]
#' ## actual fitting
#' fit.result <- LCF_solve_QP(LCF.stds = LC.standards, LCF.samp = LC.sample)
#' print(fit.result)

LCF_solve_QP <- function (LCF.stds, LCF.samp) {
  
  ## extract the names of the standards
  LC.standard.names <- colnames(LCF.stds)
    
  ## transform the raw absorption to numeric values
  X <- sapply(LCF.stds, as.numeric)
  Y <- sapply(LCF.samp, as.numeric)

  ## solve the Choleski factorization to find the covariance matrix to be minimized
  Rinv <- solve(chol(t(X) %*% X))
  
  ## create the vector to be minimized
  d <- t(Y) %*% X
  
  ## matrix with constrains under which to minimize the quadratic function (values between 0 and 1)
  C <- cbind(rep(1,length(LCF.stds)), diag(length(LCF.stds)))
  
  ## vector holding the values of sum to one contrain
  b <- c(1,rep(0,length(LCF.stds)))
  
  ## actual fit / solving the quadratic problem
  LCF.solve <- solve.QP(Dmat = Rinv, factorized = TRUE, dvec = d, Amat = C, bvec = b, meq = 1)
  
  ## extract the solution
  raw.coeff <- as.data.frame(t(LCF.solve$solution))
  colnames(raw.coeff) <- LC.standard.names
  
  ## create the fitted spectrum
  fit.spec <- rowSums(data.frame(mapply(`*`,LCF.stds,raw.coeff)))
  
  ## create the R-factor as fitting statistics
  r.fac <- sum((LCF.samp - fit.spec)^2)/sum(LCF.samp^2)
  
  ## combinde the coefficient with the R-factor
  result <- cbind(raw.coeff, R.fac = r.fac)
  
  ## return result
  return(result)
}