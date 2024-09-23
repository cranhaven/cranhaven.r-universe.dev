SSLASSO <- function(X, 
                    y, 
                    penalty = c("adaptive", "separable"), 
                    variance = c("fixed", "unknown"),
                    lambda1, 
                    lambda0,
                    nlambda = 100,
                    theta = 0.5, 
                    sigma,
                    a = 1, b, 
                    eps = 0.001, 
                    max.iter = 500, 
                    counter = 10, 
                    warn = FALSE) {
  
  # Coersion
  penalty <- match.arg(penalty)
  variance <- match.arg(variance)
  
  if (!is(X, "matrix")) {
    tmp <- try(X <- model.matrix(~0+., data=X), silent=TRUE)
    if (is(tmp, "try-error")) {
      stop("X must be a matrix or able to be coerced to a matrix")
    }
  }
  if (storage.mode(X) == "integer") {
    storage.mode(X) <- "double"
  }
  if (!is(y, "numeric")) {
    tmp <- try(y <- as.numeric(y), silent=TRUE)
    if (is(tmp,"try-error")) {
      stop("y must numeric or able to be coerced to numeric")
    }
  }
  
  if (any(is.na(y)) | any(is.na(X))) {
    stop("Missing data (NA's) detected.  Take actions (e.g., removing cases, removing features, imputation) to eliminate missing data before passing X and y to SSLASSO")
  }
  
  ## Standardize
  XX <- standard(X)
  ns <- attr(XX, "nonsingular")
  p <- ncol(XX)
  
  yy <- y - mean(y)
  n <- length(yy)
  
  if (missing(lambda0)) {
    lambda0 <- seq(1, n, length = nlambda)
    lambda1 <- lambda0[1]
  } else {
    nlambda <- length(lambda0)
    if (missing(lambda1)) {
      lambda1 <- lambda0[1]
    }
  }
  
  # Lambda0 should be an increasing sequence
  
  monotone <- sum((lambda0[-1] - lambda0[-nlambda]) > 0)
  if (monotone != nlambda - 1){
    stop("lambda0 must be a monotone increasing sequence")
  }
  if (lambda1 > min(lambda0) ) {
    stop("lambda1 must be smaller than lambda0")
  }
  
  if(missing(b)) {
    b <- p
  }
  
  # get initial value for sigma
  df <- 3
  sigquant <- 0.9
  sigest <- sd(yy)
  qchi <- qchisq(1 - sigquant, df)
  ncp <- sigest^2 * qchi / df
  min_sigma2 <- sigest^2 / n

  if (variance == "unknown") {
    if (missing(sigma)) {
      sigma <- sqrt(df * ncp / (df + 2))
    } 
  } else {
    if (missing(sigma)) {
      sigma <- sqrt(df * ncp / (df - 2))
    } 
  }

 
  
  ## Fit
  res <- .Call("SSL_gaussian", XX, yy, penalty, variance, as.double(lambda1), as.numeric(lambda0), 
               as.double(theta), as.double(sigma), as.double(min_sigma2), as.double(a), as.double(b), 
               eps, as.integer(max.iter), as.integer(counter), PACKAGE = "SSLASSO")
  bb <- matrix(res[[1]], p, nlambda)
  iter <- res[[3]]
  thetas<-res[[4]]
  sigmas <- res[[5]]
  
  ## Warning
  if (warn & any(iter == max.iter)) {
    warning("Algorithm did not converge for the ABOVE MENTIONED values of lambda0")
    print(lambda0[iter == max.iter])
  }
  
  if (iter[nlambda] == max.iter) {
    warning("Algorithm did not converge")
  }
  
  ## Unstandardize
  beta <- matrix(0, nrow = ncol(X), ncol = nlambda)
  bbb <- bb/attr(XX, "scale")[ns]
  beta[ns, ] <- bbb
  intercept <- rep(mean(y), nlambda) - crossprod(attr(XX, "center")[ns], bbb)
  
  ## Names
  varnames <- if (is.null(colnames(X))) paste("V", 1:ncol(X), sep = "") else colnames(X)
  varnames <- c(varnames)
  dimnames(beta) <- list(varnames, round(lambda0,digits=4))
  
  ## Select
  select <- apply(beta, 2, function(x){as.numeric(x!=0)})
  
  ## Model
  
  model<-(1:p)[select[,nlambda]==1]
  
  ## Output
  val <- structure(list(beta = beta,
                        intercept = intercept,
                        iter = iter,
                        lambda0 = lambda0,
                        penalty = penalty,
                        lambda1 = lambda1,
                        thetas = thetas,
                        sigmas = sigmas,
                        select = select,
                        model = model,
                        n = n),
                   class = "SSLASSO")
  
  val
}
