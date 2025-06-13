regressionStep <- function(y, X, ctrlReg = controlReg(), independence = FALSE)
  # Function for regression step in the variable selection procedure.
  # The function returns the BIC of  the multinomial logistic regression
  # of y on x after a stepwise backward selection of the predictors.
{
  # set data
  # if ( !is.factor(y) ) y <- factor(y)
  # if ( any(sapply(X, class) != "factor") ) X <- as.data.frame( lapply(X, factor) )
  dat <- cbind(y, X)
  N <- length(y)

  if ( independence ) {
    # independence assumption between clustering variables and non-clustering variables
    sel <- nnet::multinom(y ~ 1, data = dat,
                          maxit = ctrlReg$maxiter, reltol = ctrlReg$tol, trace = FALSE)
  } else {
    # regression model
    mod <- nnet::multinom(y ~ ., data = dat,
                          maxit = ctrlReg$maxiter, reltol = ctrlReg$tol, trace = FALSE)

    # selection
    sel <- suppressWarnings( MASS::stepAIC(mod, k = log(N), direction = "both", trace = FALSE) )
  }

  # out
  lk <- -sel$deviance/2
  bic <- 2*lk - sel$edf*log(N)
  attributes(bic) <- list(loglik = lk, npar = sel$edf, model = sel)
  return(bic)
}
