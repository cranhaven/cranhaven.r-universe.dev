zinb.mod <- function(target, dataset, xnew = NULL) {
  
  if ( is.null(dataset) ) {
    res <- zinb.mle(target)
 
  } else {
    dataset <- model.matrix(target ~., data.frame(dataset) )
    poia <- which(target == 0) 
    y1 <- target[-poia]
    y0 <- target[poia]
    n1 <- dim(dataset)[1] - length(poia)
    oop <- options(warn = -1)
    on.exit(oop)
    mod <- nlm(regzinb, rnorm(dim(dataset)[2] + 2), y1 = y1, y0 = y0, x = dataset, poia = poia, n1 = n1, iterlim = 5000 )
    mod <- optim(mod$estimate, regzinb, y1 = y1, y0 = y0, x = dataset, poia = poia, n1 = n1, control = list(maxit = 10000), hessian = TRUE)
    prop <- 1 / ( 1 + exp(- mod$par[1] ) )
    k <- exp( mod$par[2] )
    be <- mod$par[-c(1:2)]
    seb <- sqrt( diag( solve(mod$hessian) ) )[ -c(1:2) ]
    be <- cbind(be, seb)
    be <- cbind(be, (be[, 1]/be[, 2])^2)
    pv <- pchisq(be[, 3], 1, lower.tail = FALSE)
    be <- cbind(be, pv)
    rownames(be) <- colnames(dataset)
    colnames(be) <- c("coefficient", "std error", 
                      "chisq-statistic", "p-value")	   
    if ( !is.null(xnew) ) {
      xnew <- model.matrix(~., data.frame(xnew) )
      est <- exp( as.vector( xnew %*% be[, 1] ) ) * (1 - prop)
    } else  est <- NULL
    res <- list(be = be, prop = prop, theta = 1/k, loglik = -mod$value - sum( lgamma(y1 + 1) ), est = est )
  }

  res  
}


