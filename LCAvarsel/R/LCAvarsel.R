LCAvarsel <- function(Y, G = 1:3, X = NULL,
                      search = c("backward", "forward", "ga"),
                      independence = FALSE, swap = FALSE,
                      bicDiff = 0,
                      ctrlLCA = controlLCA(), 
                      ctrlReg = controlReg(), 
                      ctrlGA = controlGA(),
                      start = NULL,
                      checkG = TRUE,
                      parallel = FALSE, verbose = interactive())
{
  # supress warnings
  warn <- getOption("warn")
  on.exit( options("warn" = warn) )
  options("warn" = -1)
  
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
    if ( !is.data.frame(X) ) X <- as.data.frame(X)
    if ( is.null(colnames(X)) ) colnames(X) <- paste0("X", 1:ncol(X))
    if ( nrow(Y) != nrow(X) ) stop("Y and X must have the same number of observations")
    YX <- cbind(Y, X)
  } else YX <- Y
  
  # missing values
  if ( any( is.na( YX ) ) ) {
    YX <- na.exclude(YX)
    na <- attr(YX, "na.action")
    attributes(na) <- NULL
    X <- YX[,colnames(X)]
    cat("\n", "Observations with missing data have been removed \n\n")
  }

  search <- match.arg( search, c("backward", "forward", "ga") )
  
  # check starting set
  if ( !is.null(start) ) {
    if ( search == "ga" ) {
      if ( !is.matrix(start) ) stop("Starting set(s) must be provided as a binary matrix")
    } else {
      if ( !all(is.character(start)) ) stop("Starting set must be a vector of variable names")
    }
  }

  if ( search == "backward" & swap ) {
    out <- selBWD(YX[,varnames], G, ctrlLCA = ctrlLCA, ctrlReg = ctrlReg, 
                  independence = independence, swap = TRUE, covariates = X, bicDiff = bicDiff,
                  checkG = checkG, start = start, verbose = verbose, parallel = parallel)
  } else if ( search == "forward" & swap ) {
    out <- selFWD(YX[,varnames], G, ctrlLCA = ctrlLCA, ctrlReg = ctrlReg, 
                  independence = independence, swap = TRUE, covariates = X, bicDiff = bicDiff,
                  checkG = checkG, start = start, verbose = verbose, parallel = parallel)
  } else if ( search == "backward" ) {
    out <- selBWD(YX[,varnames], G, ctrlLCA = ctrlLCA, ctrlReg = ctrlReg, 
                  independence = independence, swap = FALSE, covariates = X, bicDiff = bicDiff,
                  checkG = checkG, start = start, verbose = verbose, parallel = parallel)
  } else if ( search == "forward" ) {
    out <- selFWD(YX[,varnames], G, ctrlLCA = ctrlLCA, ctrlReg = ctrlReg, 
                  independence = independence, swap = FALSE, covariates = X, bicDiff = bicDiff,
                  checkG = checkG, start = start, verbose = verbose, parallel = parallel)
  } else if ( search == "ga" ) {
    out <- selGA(YX[,varnames], G, ctrlGA = ctrlGA, ctrlLCA = ctrlLCA, ctrlReg = ctrlReg,
                 covariates = X, independence = independence,
                 checkG = checkG, start = start, parallel = parallel)
  } else stop("search method not available")

  if ( exists("na") ) out$na <- na
  class(out) <- "LCAvarsel"
  return(out)
}



print.LCAvarsel <- function(x, ...)
{
  obj <- x
  hd1 <- "Variable selection for Latent Class Analysis"
  search <- switch(obj$search,
                   "backward" = "Stepwise backward/forward greedy search",
                   "forward" = "Stepwise forward/backward greedy search",
                   "ga" = "Evolutionary search via genetic algorithm")
  hd2 <- if ( !is.null(obj$swap) ) {
    if ( obj$swap ) paste0("Swap-", search) else search
  } else search
  hd3 <- if ( obj$independence ) {
    "with global independence assumption"
  } else "with conditional dependence via regression"
  
  sep <- paste0( rep("=", max(nchar(hd1), nchar(hd2)) + 5), collapse = "" ) 
  cat("\n", " ", hd1, "\n")
  cat(sep, "\n")
  cat(" ", hd2, "\n")
  cat(" ", hd3, "\n")
  cat(sep, "\n", "\n")
  
  cat(" Selected variables:", obj$variables, "\n" )
  cat(" Clustering model:", "LCA model with", obj$model$G, "latent classes", "\n")
  if ( !is.na(obj$model$coeff[1]) ) cat("                  ", "Covariates:", 
                                        paste(rownames(obj$model$coeff)[-1], collapse = " + "), "\n")
  cat("\n")
  if ( obj$search != "ga" ) {
    cat(" Steps:", "\n")
    colnames(obj$info)[2] <- "Proposed variable"
    print(obj$info)
  } else {
    cat(" Summary:")
    sm <- obj$GA@summary[nrow(obj$GA@summary),]
    dt <- data.frame( sm[c(1,2,4,6)] )
    rownames(dt) <- c(" Best = ", " Mean = ", " Median = ", " Min = ")
    colnames(dt) <- ""
    print(dt)
  }
}

