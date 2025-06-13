#' Estimates delta for Iterated Stable Autoencoder
#'
#' This function uses cross-validation to estimate delta for the Iterated Stable Autoencoder when considering Binomial noise. delta is the probability of deletion of each cell of
#'              the data matrix  
#' 
#' @param X a data frame or a matrix with count  
#' @param delta vector, a sequence of values for the probability of deletion of each cell of
#'              the data matrix
#' @param noise noise model assumed for the data. By default and only available "Binomial" 
#' @param transformation estimates a transformation of the original matrix; currently,
#'                       only correspondence analysis CA is available
#' @param pNA percentage of missing values added in the data set
#' @param nbsim number of times that pNA values are inserted and predicted in the data
#' @param maxiter integer, maximum number of iterations of the iterative imputation algorithm
#' @param threshold for assessing convergence of the iterative imputation algorithm (difference between two successive iterations) 
#'
#' @return msep, matrix with the MSEP obtained for each simulation and each value of delta
#' @return delta, value giving in average the smallest MSEP over the nbsim simulations
#'
#' @details  For each value delta, repeated learning cross-validation consists in inserting pNA percentage of missing values in the data set and predicting them with the Iterative Stable Autoencoder. More precisely, the prediction is obtained using the iterative imputation algorithm (imputecount) which alternates steps of imputation of the missing entries and estimation of the low-rank signal.   
#' This process is repeated nbsim times for all the deltas. The mean squared error of prediction is kept for each simulation and value of delta. The value of delta leading to the smallest MSEP on average over the simulations is given. 
#' @seealso \code{\link{imputecount}}
#' @seealso \code{\link{ISA}}
#' @examples 
#'  # A regularized Correspondence Analysis 
#'  \dontrun{library(FactoMineR)
#'  perfume <-  read.table("http://factominer.free.fr/docs/perfume.txt",header=TRUE,
#'  sep="\t",row.names=1)
#'  rownames(perfume)[4] <- "Cinema"
#'  
#'  isa.delt <- estim_delta(perfume, nbsim = 10, transformation = "CA")
#'  
#'  isa.ca <- ISA(perfume, delta = isa.delt$delta, noise = "Binomial", transformation = "CA")
#'  rownames(isa.ca$mu.hat) <- rownames(perfume)
#'  colnames(isa.ca$mu.hat) <- colnames(perfume)
#'  res.isa.ca <- CA(isa.ca$mu.hat, graph = FALSE)
#'  plot(res.isa.ca, title = "Regularized CA", cex = 0.6, selectCol = "contrib 20")}


## estim_delta
estim_delta <- function(X, 
                        delta = seq(0.1, 0.9, length.out = 9), 
                        nbsim = 10,  
                        noise = "Binomial",
                        transformation = c("None", "CA"), 
                        pNA = 0.10,
                        maxiter = 1000, 
                        threshold = 1e-08){
  
  if(inherits(X, "data.frame")){
    X <- as.matrix(X)
  }
  
  if(sum(sapply(X, is.numeric)) < ncol(X)){
    stop("all the variables must be numeric")
  }
  
  noises <- c("Gaussian", "gaussian", "Binomial","binomial")
  noise <- match.arg(noise, noises, several.ok = T)[1]
  noise <- tolower(noise)
  
  if(noise == "gaussian"){
   stop("estim_delta is defined only for binomial noise")
  }
  
   res.msep  <- matrix(NA,  nbsim, length(delta))       
  
   if (transformation == "CA" ) {
    R <- rowSums(X, na.rm = T)
    C <- colSums(X, na.rm = T)
     if (prod(R) * prod(C) == 0) {
       stop("to run CA, each row and columns must have a positive number of counts")
     }
    indep.term <- 1/sum(X, na.rm= T) * matrix(R, length(R), 1) %*%  matrix(C, 1, length(C))
    Xc <-  sweep(sweep(X - indep.term, MARGIN = 1, FUN = "*",(1/sqrt(R))) ,  MARGIN = 2, FUN = "*", (1/sqrt(C)))
    
   } else {
    Xc <- X
   }
   
   # Run the cross-validation: insertion of missing values, prediction with imputecount and computation of the MSEP
   for (sim in 1:nbsim) {
    XNA <- as.matrix(X)
    indNA <- sample(1:(nrow(XNA) * ncol(XNA)), round(pNA *  nrow(XNA) * ncol(XNA), 0)) 
    XNA[indNA] <- NA   
    for (ll in 1:length(delta)) {
      res.isa <- imputecount(XNA, delta = delta[ll], transformation = transformation, maxiter = maxiter,  threshold = threshold)
      XX <- res.isa$mu.hat
     if(transformation == "CA") {         
       RR <- rowSums(res.isa$mu.hat)
       CC <- colSums(res.isa$mu.hat)
       if (prod(RR) * prod(CC) == 0) {
        XX <- matrix(0, nrow(XNA), ncol(XNA))
       } else {
       indep.term2 <- 1/sum(res.isa$mu.hat) * matrix(RR, length(RR), 1) %*%  matrix(CC, 1, length(CC))
       XX <- diag(1/sqrt(RR)) %*% (XX - indep.term2) %*% diag(1/sqrt(CC))  
       }
    } 
       res.msep[sim, ll] <- mean((XX[indNA]-Xc[indNA])^2, na.rm = T)       
    }
   } 
  colnames(res.msep) <- c(delta)  
  result      <- list(delta = which.min(apply(res.msep, 2, median)), msep = res.msep)
  return(result)
}