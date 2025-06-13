formulaLCA <- function(variables, covariates = NULL)
{
  if ( is.null(covariates) ) {
    f <- paste( paste("cbind(", paste(variables, sep = "", collapse = ","), ")", sep = ""), "~1", sep = "" )
  } else {
    f <- paste( paste("cbind(", paste(variables, sep = "", collapse = ","), ")", sep = ""),
                paste(covariates, collapse = "+"), sep = "~" )
  }

  return( formula(f) )
}



fitLCA <- function( Y, G = 1:3, X = NULL, ctrlLCA = controlLCA() )
{
  # response variables
  if ( is.null(colnames(Y)) ) colnames(Y) <- paste0("Y", 1:ncol(Y))
  cl <- sapply(Y, class)
  if ( any(cl != "factor") ) {
    # convert response variables to factor
    set <- which(cl == "numeric" | cl == "integer" | cl == "character") 
    Y[,set] <- as.data.frame( lapply(Y[,set], factor) )
  }
  varnames <- colnames(Y)
  
  # covariates
  if ( !is.null(X) ) {
    # suppressMessages( require(MASS) )   # need 'ginv' function in MASS
    if ( !is.data.frame(X) ) X <- as.data.frame(X)
    if ( is.null(colnames(X)) ) colnames(X) <- paste0("X", 1:ncol(X))
    if ( nrow(Y) != nrow(X) ) stop("Y and X must have the same number of observations")
    YX <- cbind(Y, X)
  } else YX <- Y
  covariates <- colnames(X)
  
  # formula
  form <- formulaLCA(varnames, covariates)

  # fit
  fit <- lapply( G, function(g)
  { poLCA::poLCA(form, data = YX, nclass = g,
                 maxiter = ctrlLCA$maxiter, tol = ctrlLCA$tol, nrep = ctrlLCA$nrep,
                 verbose = FALSE, calc.se = FALSE) 
  } )
  
  # compute bic as 2*loglik - npar*log(N)
  bic <- 2*sapply(fit, "[[", "llik") - sapply(fit, "[[", "npar")*log( nrow(Y) )
  BIC <- bic; names(BIC) <- G

  # best model
  best <- which.max(bic)
  mod <- fit[[best]]

  # output
  out <- list( G = length(mod$P), parameters = list(theta = mod$probs, tau = mod$P),
               coeff = mod$coeff, loglik = mod$llik, bic = bic[best], npar = mod$npar, 
               resDf = mod$resid.df, z = mod$posterior, class = mod$predclass, 
               BIC = BIC, iter = mod$numiter )
  if ( out$resDf < 0 ) warning("Negative degrees of freedom, respecify model")
  class(out) <- "fitLCA"
  return(out)
}



print.fitLCA <- function(x, ...)
  # a simple print method
{
  obj <- x    # I like obj
  if ( is.na(obj$coeff[1]) ) {
    cat("LCA model with", obj$G, "latent classes")
  } else {
    cat("LCA model with", obj$G, "latent classes \n")
    cat( "Covariates:", paste(rownames(obj$coeff)[-1], collapse = " + ") )
  }
}