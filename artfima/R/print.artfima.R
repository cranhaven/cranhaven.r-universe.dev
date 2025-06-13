print.artfima <-
function(x, ...){
  glp <- x$glp
  p <- x$arimaOrder[1]
  d <- x$arimaOrder[2]
  q <- x$arimaOrder[3]
  est <- numeric(0)
  estT <- character(0)
  glpOrder <- x$glpOrder
  est <- c(est, x$constant)
  estT <- c(estT, ifelse(d==0,"mean","constant"))
  if (glpOrder==2) {
    est <- c(est, x$dHat) #d is first
    estT <- c(estT, "d")
    est <- c(est, x$lambdaHat)
    estT <- c(estT, "lambda")
  } else {
    if(glpOrder==1) {
      est <- c(est, x$dHat)
      estT <- c(estT, "d")
    }
  }
  if(p>0) {
    est <- c(est, x$phiHat)
    estT <-c(estT, paste0("phi(", paste0(1:p, ")")))
  }
  if(q>0) {
    est <- c(est, x$thetaHat)
    estT <- c(estT, paste0("theta(", paste0(1:q, ")")))      
  }
  whichModel <- paste0(x$glp, "(", p, ",", d, ",", q, ")" )
  cat(paste0(whichModel, ", MLE Algorithm: ", x$likAlg, ", optim: ",x$optAlg),
      fill=TRUE)
  cat(paste0("snr = ", round(x$snr,3), ", sigmaSq = ", 
             x$sigmaSq), fill=TRUE)
  if (x$convergence!=0) {
    cat(paste0("Note: possible problem with convergence\n  convergence=",
               x$convergence), fill=TRUE)
    if (!is.null(x$message)) 
      cat(paste0("message =", x$message), fill=TRUE)
  }
  k <- length(est)
  LL <- x$LL
  aic <- -2*LL + 2*(k+1) #add 1 for mean to agree with arima()
  bic <- -2*LL + (k+1)*log(x$n)
  cat(paste0("log-likelihood = ", round(LL,2), ", AIC = ", round(aic,2), 
      ", BIC = ", round(bic,2)),  fill=TRUE)
  if (x$onBoundary) {
    cat("Warning: estimates converged to boundary!", fill=TRUE)
  }
  print(matrix(c(est, x$seMean, x$se), ncol=2, 
         dimnames=list(estT, c("est.", "se(est.)"))))   
}
