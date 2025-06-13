#' Adaptive Shrinkage with missing values - Imputation
#'
#'This function estimates a low-rank signal from a noisy Gaussian incomplete data using the iterative Adaptive Trace Norm (ATN) algorithm. It can be used to impute a data set.
# In the iterative ATN algorithm, singular values are transformed using a function indexed by two parameters lambda and gamma as
#' dl  = dl * max(1-(lambda/dl)^gamma,0). If, the parameters lambda and gamma are not specified,  they are estimated by minimizing a Missing Stein unbiased risk estimate (SURE) when the variance sigma^2 of the noise is known or a generalized SURE (GSURE) otherwise.
#' These SURE and GSURE for missing values are implemented using finite differences. 
#' 
#' @param X a data frame or a matrix with numeric entries
#' @param lambda integer, value to be used in the iterative ATN algorithm
#' @param gamma integer, value to be used in the iterative ATN algorithm
#' @param nb.init integer, to run the iterative ATN algorithm with nbinit different initialization. By default 1.
#' @param threshold, for assessing convergence (difference between two successive iterations) 
#' @param maxiter integer, maximum number of iterations of the iterative imputation algorithm
#' @param method to select the two tunning parameters lambda and gamma. By default by minimizing GSURE
#' @param sigma integer, standard deviation of the Gaussian noise. 
#' @param gamma.seq a vector for the sequence of gamma. The values must be greater than 1
#' @param method.optim the method used in the optim function. By default BFGS
#' @param center boolean, to center the data. By default "TRUE"
#' @param scale boolean, to scale the data. By default "FALSE"
#' @param lambda0 integer, the initial value for lambda used to optimize SURE and GSURE. By default the median of the singular values (must be in log scale)
#' @return mu.hat the estimator of the signal
#' @return completeObs the completed data set. Observed values are the same but missing values are replaced by the estimated one in mu.hat
#' @return nb.eigen the number of non-zero singular values
#' @return gamma the given gamma or the optimal gamma selected by minimizing SURE or GSURE
#' @return lambda the given lambda or the optimal lambda selected by minimizing SURE or GSURE
#' @return singval the singular values of the estimator
#' @return low.rank the results of the SVD of the estimator
#' @details The iterative ATN algorithm first consists in imputing missing values with initial values. 
#' Then, adashrink is performed on the completed dataset with its regularization parameter lambda and gamma. The missing entries are imputed with the estimated signal. These steps of estimation of the signal via adashrink and imputation of the missing values are iterated until convergence. At the end, both an estimation of the signal and a completed data set are provided. If lambda and gamma are not known, they can be estimated by minimizing SURE when sigma^2 is known. To do this, a grid for gamma is defined in gamma.seq (gammas must be greater than 1) and the Missing SURE function is optimized on lambda using the optim function of the package stats (?optim) with the optimization method by default sets to "BFGS". The initial lambda can be modified in the argument lambda0.  
#' When sigma is not known, it is possible to estimate the two tuning parameters by minimizing Missing GSURE. Note that Missing SURE is defined using finite differences so it is computationally costly.
#' The estimated low rank matrix is given in the output mu.hat. imputeada automatically estimates the rank of the signal. Its value is given in the output nb.eigen corresponding to the number of non-zero eigenvalues. 
#' @seealso \code{\link{adashrink}}
#' @seealso \code{\link{LRsim}}
#' @examples 
#' don.NA <- LRsim(200, 500, 100, 4)$X
#' don.NA[sample(1:(200*500),20, replace = FALSE)] <- NA
#' \dontrun{adaNA <- imputeada(don.NA, lambda = 0.022, gamma = 2.3)
#' esti <- adaNA$mu.hat
#' comp <- adaNA$completeObs}

imputeada <- function(X,
                        lambda = NA,
                        gamma = NA,
                        sigma = NA,
                        method = c("GSURE", "SURE"), 
                        gamma.seq = seq(1, 5, by=.1),
                        method.optim = "BFGS",
                        center = "TRUE",
                        scale = "FALSE",
                        threshold = 1e-8,
                        nb.init = 1,
                        maxiter = 1000,
                        lambda0 = NA){
  
  
  # housekeeping
  
  if(inherits(X, "data.frame")){
    X <- as.matrix(X)
  }
  
  if(sum(sapply(X, is.numeric)) < ncol(X)){
    stop("all the variables are not numeric")
  }
  
  if(!is.na(sigma) & (sigma <= 0)){
    stop("sigma must be positive")
  }
  
  if(sum(gamma.seq<1)>0){
    stop("the gammas may be greater or equal to 1")
  }
  
  missing <- which(is.na(X))
  
  if(length(missing) ==  0){
    stop("there are no missing values")
  }
  
  method <- match.arg(method, c("GSURE", "Gsure", "gsure", "gSure","GSure", "SURE", "sure", "Sure"), several.ok = T)[1]
  method <- tolower(method)
  
  ## Definition of the adashrinkNA function to impute the data according to the adashrink estimator.
  
  adashrinkNA <- function (X, 
                         center = TRUE,
                         scale = FALSE, 
                         threshold = 1e-08,
                         seed = NULL, 
                         nb.init = 1,
                         maxiter = 1000, 
                         lambda = lambda, 
                         gamma = gamma,
                         ...) {
    
    adashrinkNAcore <- function(X, 
                              center = center,
                              scale = scale,
                              threshold = threshold, 
                              seed = NULL, 
                              init = 1, 
                              maxiter = maxiter, 
                              lambda = lambda,
                              gamma = gamma,
                              ...) {
      
      # housekeeping
      
      if(inherits(X, "data.frame")){
        X <- as.matrix(X)
      }
      
      if(sum(sapply(X, is.numeric)) < ncol(X)){
        stop("all the variables are not numeric")
      }
      
      missing <- which(is.na(X))
      if(length(missing) ==  0){
        stop("there are no missing values")
      }
      
      nb.iter <- 1
      old <- Inf
      objective <- 0
      if (!is.null(seed)) {
        set.seed(seed)
      }
      
      # Initialization: centering, scaling and missing values imputed by 0 or a random draw gaussian (0,1)
      if (center){
        mean.p <- apply(X, 2, mean, na.rm = T)
        Xhat <- sweep(X, 2, mean.p, FUN = "-")
      }else{
        Xhat <- X
      }
      if (scale){  
        et <- apply(Xhat, 2, sd, na.rm = T)
        Xhat <- sweep(Xhat, 2, et, FUN = "/")
      }
      
      Xhat[missing] <- 0
      if (init > 1) {
        Xhat[missing] <- rnorm(length(missing))
      }
      
      recon <- Xhat
      
      while (nb.iter > 0) {
        
        #imputation    
        Xhat[missing] <- recon[missing]
        
        #updating mean and sd
        if (scale)  Xhat = sweep(Xhat, 2, et, FUN = "*")
        if (center) Xhat <- sweep(Xhat, 2, mean.p, FUN = "+")
        if (center){
          mean.p <- apply(Xhat, 2, mean) 
          Xhat <- sweep(Xhat, 2, mean.p, FUN = "-")
        }
        if (scale) {
          et <- apply(Xhat, 2, sd)
          Xhat <- sweep(Xhat, 2, et, FUN = "/")
        }
        # estimation
        svdX <- svd(Xhat)
        dhat <- svdX$d * pmax( 1-(lambda/svdX$d)^gamma, 0) 
        temp <- t(t(svdX$u)*dhat)
        recon <- temp %*% t(svdX$v)
        
        diff <- Xhat - recon
        diff[missing] <- 0
        row.w = rep(1, nrow(X))/nrow(X)
        objective <- sum(sweep(diff^2, 1, row.w, FUN = "*"))
        criterion <- abs(1 - objective/old)
        old <- objective
        nb.iter <- nb.iter + 1
        if (!is.nan(criterion)) {
          if ((criterion < threshold) && (nb.iter > 5)) 
            nb.iter <- 0
          if ((objective < threshold) && (nb.iter > 5)) 
            nb.iter <- 0
        }
        if (nb.iter > maxiter) {
          nb.iter <- 0
          warning(paste("Stopped after ", maxiter, " iterations"))
        }
      }
      
      if (scale)  Xhat <- sweep(Xhat, 2, et, FUN = "*")
      if (center) Xhat <- sweep(Xhat, 2, mean.p, FUN = "+")
      
      completeObs <- X
      completeObs[missing] <- Xhat[missing]
      if (scale) recon <- sweep(recon, 2, et, FUN = "*")
      if (center) recon <- sweep(recon, 2, mean.p, FUN = "+")
      result <- list()
      row.names(completeObs) <- row.names(X)
      colnames(completeObs) <- colnames(X)
      row.names(recon) <- row.names(X)
      colnames(recon) <- colnames(X)
      nbeig <- sum(svdX$d > lambda)
      result$completeObs <- completeObs
      result$mu.hat <- recon
      result$low.rank <- svdX
      result$low.rank$d <-dhat
      result$singval <- dhat
      result$nb.eigen  <- nbeig
      return(result)
    }
    obj = Inf
    
    for (i in 1:nb.init) {
      res.impute = adashrinkNAcore(X, scale = scale, center = center, 
                                 threshold = threshold, lambda = lambda, gamma = gamma, seed = if (!is.null(seed)) {
                                   (seed * (i - 1))
                                 }
                                 else {
                                   NULL
                                 }, init = i, maxiter = maxiter)
      if (mean((res.impute$mu.hat[!is.na(X)] - X[!is.na(X)])^2) < 
          obj) {
        res <- res.impute
        obj <- mean((res.impute$mu.hat[!is.na(X)] - X[!is.na(X)])^2)
      }
    }
    return(res)
  }
  
  
  
  ## Definition of the SURENA(lambda, gamma) cost function. The divergence will be estimated using finite differences.
  SURENA.cost <- function(X,
                          method.ada,
                          center,
                          scale,
                          sigma,
                          nb.init,
                          threshold,
                          maxiter,
                          gamma, 
                          lambda
  ){
    n <- nrow(X)
    p <- ncol(X)
    lambda <- exp(lambda) 
    
    ## compute the estimator with missing values with the function adashrinkNA
    
    res.impute <- adashrinkNA(X, lambda = lambda, gamma = gamma, center = center, scale = scale, threshold = threshold, nb.init = nb.init)$mu.hat
    missing <- which(is.na(X))
    esti <- matrix(NA, n, p)
    epsilon <- 1e-5
    for (i in 1:(n*p) ){
      Xt <- X
      if(!is.na(Xt[i])){
        Xt[i] <- (X+epsilon)[i] 
        esti[i] <- (adashrinkNA(Xt, lambda = lambda, gamma = gamma, center = center, scale = scale, threshold = threshold)$mu.hat)[i]
      }
      
    }
    
    divestime <- sum((esti-res.impute)[-missing])/epsilon # estimation of the divergence
    RSS <- sum(((X-res.impute)[-missing])^2) # sum of squared of the residuals on the observed values
    
    if(method.ada == "gsure") {
      mse.ada <- RSS/(1-divestime/(n*p-length(missing)))^2
    } else{       
      mse.ada <- -(n*p-length(missing))*sigma^2 + RSS  + 2*sigma^2 *divestime
    }
    return(mse.ada)
  }
  
  ###############
  ## Begin
  # infer unspecified choices
  
  if ((!is.na(lambda))& (!is.na(gamma)))
  {
    lambda.o = lambda
    gamma.o = gamma
  } else{
      
  if(is.na(sigma)&(method != "gsure")){   
    warning("since sigma was not specified, GSURE is used")
    method = "gsure" 
  }
  
  val.optglob <- Inf
  
  n <- nrow(X)
  p <- ncol(X)
  missing = which(is.na(X))
  
  # initialization value for lambda
  Xmean = X
  Xmean[missing] = 0
  svdXmean <- svd(Xmean)
  svdXdmean <- svdXmean$d	
  
  gamma.seq <- gamma.seq
  if(is.na(lambda0)){
    lambda0 <- log(median(svdXdmean))
  }  
  
  if(length(gamma.seq) == 1) {
    lambda.o <- optim(lambda0, SURENA.cost, X = X,  method.ada = method, sigma = sigma, gamma = gamma.seq, method = method.optim, center = center, scale = scale, nb.init = nb.init, threshold = threshold, maxiter = maxiter, control = list(maxit=20, reltol = 1e-6), lower = 0.0001*lambda0)$par
    lambda.o <- exp(lambda.o)
    gamma.o <- gamma.seq
  }else{
    for (gamma in gamma.seq){
    
      res.opti <- optim(lambda0, SURENA.cost, X = X, sigma = sigma, nb.init = nb.init, threshold = threshold, maxiter = maxiter, gamma = gamma,    method.ada = method, method = method.optim,  center = center,  scale = scale,  control = list(maxit=20, reltol = 1e-6), lower = 0.0001*lambda0)
      lambda.temp <- exp(res.opti$par)
      val.opt <- res.opti$val
      if (val.opt < val.optglob) {	
        gamma.o <- gamma
        lambda.o <- lambda.temp
        val.optglob <- val.opt
      }
    }
  } 
}
  res.impute <- adashrinkNA(X, lambda = lambda.o, gamma = gamma.o, center = center, scale = scale, threshold = threshold, nb.init = nb.init, maxiter = maxiter)
  
  return(list(mu.hat = res.impute$mu.hat,
              completeObs = res.impute$completeObs,
              nb.eigen = res.impute$nb.eigen,
              gamma = gamma.o,
              lambda = lambda.o, 
              singval = res.impute$singval,
              low.rank = res.impute$low.rank))
}




 





