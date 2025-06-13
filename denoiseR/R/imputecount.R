#' Imputation of count data with the Iterated Stable Autoencoder
#'
#' This function estimates a low-rank signal from a noisy count incomplete data
#' using the Iterated Stable Autoencoder. It can be used to impute a data set.
#' 
#' @param X a data frame or a matrix with count data containing missing values
#' @param delta numeric, probability of deletion of each cell of
#'              the data matrix. By default delta = 0.5
#' @param transformation estimate a transformation of the original matrix; currently,
#'                       only correspondence analysis CA is available
#' @param maxiter integer, maximum number of iterations of the iterative imputation algorithm
#' @param threshold for assessing convergence (difference between two successive iterations) 
#'
#' @return mu.hat the estimator of the signal
#' @return completeObs the completed data set. The observed values are kept for the non-missing entries and the missing values are replaced by the predicted ones
#'
#' @details Impute the missing entries of a count data set using the iterative ISA algorithm.
#' The iterative ISA algorithm first consists in imputing missing values with initial values. 
#' Then, ISA is performed on the completed dataset with its regularization parameter delta. The missing entries are imputed with the estimated signal. These steps of estimation of the signal via ISA and imputation of the missing values are iterated until convergence. 
#' @seealso \code{\link{ISA}}
#' @examples 
#' #

## imputecount
imputecount <- function (X,
                         threshold = 1e-08, 
                         maxiter = 1000,
                         delta = 0.5,  
                         transformation = c("None", "CA")) {  
  nb.iter <- 1
  old <- Inf
  objective <- 0
  
  X <- as.matrix(X)
  
  if(sum(is.na(X)) == 0){
    stop("No value is missing")
  }
  
  missing <- which(is.na(X))
  
  # Initialization: missing values are imputed with random values
  Xhat <- X
  Xhat[missing] <- sample(X[-missing],length(missing)) + 1
  recon <- Xhat

    while (nb.iter > 0) {
      Xhat[missing] <- recon[missing]
      RXhat <- rowSums(Xhat)
      CXhat <- colSums(Xhat)
      if ((sum(RXhat>1e-6) == nrow(Xhat)) & (sum(CXhat>1e-6) == ncol(Xhat))){
        res.isa <- ISA(Xhat, noise = "Binomial", delta = delta,  transformation = transformation)
        recon <- res.isa$mu.hat     
      }
               
      diff <- Xhat - recon
      diff[missing] <- 0
      objective <- sum((diff^2))
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
  
    completeObs <- X
    completeObs[missing] <- Xhat[missing]
    result <- list()
    result$completeObs <- completeObs
    result$mu.hat <- recon
    return(result)
  }
   
