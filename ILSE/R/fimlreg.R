fimlreg <- function(...) UseMethod("fimlreg")
fimlreg.numeric <- function(Y, X, ...){
  if(is.null(Y) || is.null(X)) stop('"X","Y" must be given
                                    simutaneously!')
  if(is.null(n <- nrow(X)))
    stop("'X' must be a matrix")
  if (n == 0L)
    stop("0 (non-NA) cases")
  if(!is.matrix(X)) X <- as.matrix(X)
  data <- cbind(Y,X)
  resEst <- mvnest(data)
  p1 <- length(resEst$muhat)
  muhat <- resEst$muhat
  sigmahat <- resEst$sigmahat
  beta2 <- as.vector(sigmahat[1,2:p1]%*% qr.solve(sigmahat[2:p1,2:p1]))
  beta1 <- muhat[1] - as.vector(sigmahat[1,2:p1]%*% qr.solve(sigmahat[2:p1,2:p1])%*% matrix(muhat[2:p1],nrow=p1-1))
  beta <- c(beta1, beta2)
  names(beta)[1] <- 'intercept'
  names(beta)[2:p1] <- names(data)[2:(p1)]
  return(list(beta=beta))
}
fimlreg.formula <- function(formula, data=NULL, ...){
  # require(stats)
  if(is.null(data)){
    ## generate data from the current environment
    data <- model.frame(formula = formula, na.action=NULL)
  }
  ## obtain name of response variable
  form <- terms(formula, data=data)
  vars <- attr(form, "variables")
  resp <- row.names(attr(form, "factors"))[1]

  ## obtain design matrix
  if("(Intercept)" %in% colnames(data)) data$`(Intercept)` <- NULL
  Xmat <- model.matrix.lm(object = formula, data=data, na.action = "na.pass")
  # XYdat <- model.frame(formula = formula, data = data, na.action=NULL)
  XYdat <- cbind(data[[resp]], Xmat)
  colnames(XYdat)[1] <- resp
  data <- as.data.frame(XYdat)
  if("(Intercept)" %in% colnames(data)) data$`(Intercept)` <- NULL
  # print(data[1:3,])
  resEst <- mvnest(data)
  p1 <- length(resEst$muhat)
  muhat <- resEst$muhat
  sigmahat <- resEst$sigmahat
  beta2 <- as.vector(sigmahat[1,2:p1]%*% qr.solve(sigmahat[2:p1,2:p1]))
  beta1 <- muhat[1] - as.vector(sigmahat[1,2:p1]%*% qr.solve(sigmahat[2:p1,2:p1])%*% matrix(muhat[2:p1],nrow=p1-1))
  beta <- c(beta1, beta2)
  names(beta)[1] <- 'intercept'
  names(beta)[2:p1] <- names(data)[2:(p1)]
  res <- list(beta=beta,
              formula=formula, data=data)
  class(res) <- 'fiml'
  return(res)

}

## estimate the mean  and covariance by using all obseved values under MCAR.
mvnest <- function(YX){
   hmu <- colMeans(YX, na.rm=TRUE)
   hsigma <- cov(YX, use = 'pairwise.complete.obs')
   return(list(muhat = hmu, sigmahat=hsigma) )
}

