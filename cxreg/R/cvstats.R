cvstats <- function(cvstuff,foldid,nfolds,lambda){

  cvm <- with(cvstuff,apply(cvraw, 2, weighted.mean, w = weights, na.rm = TRUE))
  cvsd <- with(cvstuff, sqrt(apply(scale(cvraw, cvm, FALSE)^2, 2, weighted.mean,
                                w = weights, na.rm = TRUE)/(N - 1)))
  nas <- is.na(cvsd)
  if(any(nas)){
    lambda <- lambda[!nas]
    cvm <- cvm[!nas]
    cvsd <- cvsd[!nas]
  }

  list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvup = cvm + cvsd, cvlo = cvm - cvsd)
}
