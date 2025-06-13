#' Iterated Stable Autoencoder
#'
#' This function estimates a low-rank signal from noisy data
#' using the Iterated Stable Autoencoder. More precisely, it transforms
#' a noise model into a regularization scheme using a parametric bootstrap. 
#' In the Gaussian noise model, the procedure is equivalent to shrinking the
#' singular values of the data matrix (a non linear transformation of the singular
#' values is applied) whereas it gives other estimators with rotated singular vectors outside the Gaussian framework.
#' Within the framework of a Binomial or Poisson noise model, it is also possible
#' to find the low-rank approximation of a transformed version of the data matrix
#' for instance such as the one used in Correspondence Analysis.
#' 
#' @param X a data frame or a matrix with numeric entries
#' @param sigma numeric, standard deviation of the Gaussian noise. By default sigma is estimated using the estim_sigma function with the MAD option 
#' @param delta numeric, probability of deletion of each cell of
#'              the data matrix when considering Binomial noise. By default delta = 0.5
#' @param noise noise model assumed for the data. By default "Gaussian" 
#' @param transformation estimate a transformation of the original matrix; currently,
#'                       only correspondence analysis is available
#' @param svd.cutoff singular values smaller than this are treated as numerical error
#' @param maxiter integer, maximum number of iterations of ISA  
#' @param threshold for assessing convergence (difference between two successive iterations) 
#' @param nu integer, number of singular values to be computed - may be useful for very large matrices 
#' @param svdmethod svd by default. irlba can be specified to use a fast svd method.
#'                  It can be useful to deal with large matrix. In this case, nu may be specified
#' @param center boolean, to center the data for the Gaussian noise model. By default "TRUE"
#'
#' @return mu.hat the estimator of the signal
#' @return nb.eigen the number of non-zero singular values
#' @return low.rank the results of the SVD of the estimator; for correspondence analysis, returns
#'                  the SVD of the CA transform
#' @return nb.iter number of iterations taken by the ISA algorithm
#'
#' @details When the data are continuous and assumed to be drawn from a Gaussian distribution
#' with expectation of low-rank and variance sigma^2, then ISA performs a regularized SVD by
#' corrupting the data with an homoscedastic Gaussian noise (default choice) with variance sigma^2.
#' A value for sigma has to be provided. When sigma is not known, it can be estimated using the
#' function estim_sigma.  
#' 
#' For count data, the subsampling scheme used to draw X can be considered as Binomial or Poisson (equivalent to
#' Binomial, delta = 0.5). ISA regularizes the data by corrupting the data with Poisson noise or by drawing
#' from a Binomial distribution of parameters X_ij and 1-delta divided by 1-delta. Thus it is necessary to give a value
#' for delta. When, the data are transformed with Correspondence Analysis (transfo = "CA"), this latter
#' noising scheme is also applied but on the data transformed with the CA weights.
#' The estimated low rank matrix is given in the output mu.hat. ISA automatically estimates the rank of the signal.
#' Its value is given in the output nb.eigen corresponding to the number of non-zero eigenvalues.
#'
#' @references Josse, J. & Wager, S. (2016). Bootstrap-Based Regularization for Low-Rank Matrix Estimation. Journal of Machine Learning Research.
#'
#' @seealso \code{\link{estim_sigma}}
#' @seealso \code{\link{LRsim}}
#' @examples 
#' Xsim <- LRsim(200, 500, 10, 4)
#' isa.gauss <- ISA(Xsim$X, sigma = 1/(4*sqrt(200*500)))
#' isa.gauss$nb.eigen
#' 
#' # isa.bin <- ISA(X, delta = 0.7, noise = "Binomial")
#' 
#' # A regularized Correspondence Analysis 
#' \dontrun{library(FactoMineR)
#'  perfume <-  read.table("http://factominer.free.fr/docs/perfume.txt",
#'  header=TRUE,sep="\t",row.names=1)
#'  rownames(perfume)[4] <- "Cinema"
#'  isa.ca <- ISA(perfume, delta = 0.5, noise = "Binomial", transformation = "CA")
#'  rownames(isa.ca$mu.hat) <- rownames(perfume)
#'  colnames(isa.ca$mu.hat) <- colnames(perfume)
#'  res.isa.ca <- CA(isa.ca$mu.hat, graph = FALSE)
#'  plot(res.isa.ca, title = "Regularized CA", cex = 0.6, selectCol = "contrib 20")
#'  res.ca <- CA(perfume, graph = FALSE)
#'  plot(res.ca, title = "CA", cex = 0.6, selectCol = "contrib 20")}


## ISA 
ISA <- function (X,
                 sigma = NA,
                 delta = NA,
                 noise = c("Gaussian", "Binomial"), 
                 transformation = c("None","CA"),
                 svd.cutoff = 0.001,
                 maxiter = 1000,
                 threshold = 1e-06,
                 nu = min(nrow(X), ncol(X)),
                 svdmethod = c("svd", "irlba"),
                 center = TRUE){   
  
  # housekeeping
  
  if(inherits(X, "data.frame")){
    X <- as.matrix(X)
  }
   
  if(sum(sapply(X, is.numeric)) < ncol(X)){
    stop("all the variables must be numeric")
  }
  
  if(!is.na(sigma) & sigma <= 0){
    stop("sigma must be positive")
  }
  
  if(!is.na(delta) & (delta < 0 | delta > 1)){
    stop("delta must be between 0 and 1")
  }
  
  noises <- c("Gaussian", "gaussian", "Binomial","binomial")
  noise <- match.arg(noise, noises, several.ok = T)[1]
  noise <- tolower(noise)
  
  transformations <- c("no", "No", "NO", "none", "None", "NONE", "CA", "Ca", "ca")
  transformation <- match.arg(transformation, transformations, several.ok = T)[1]
  transformation <- substr(tolower(transformation), 1, 2)
  
  methods <- c( "svd", "irlba", "SVD", "IRLA", "Svd", "Irlba")
  svdmethod <- match.arg(svdmethod, methods, several.ok = T)[1]
  svdmethod <- tolower(svdmethod)
  
  # infer unspecified choices
  
  if(transformation == "ca" & noise == "gaussian"){
    warning("the CA transform was specified, so using a binomial noise model")
    noise <- "binomial"
  }  
  
  
  if(is.na(delta)) {
    delta <- 0.5; 
  }
  
  if(is.na(sigma) & noise == "gaussian"){
    sigma <- estim_sigma(X, method = "MAD")
    warning(paste("sigma estimated by MAD:", round(sigma,6)))
  }  
 
  #
  # Solve the iterated stable autoencoder
  #
  
  # Set up the problem
  
  # For computational efficiency, we always encode rows if p <= n
  # and columns else
  take.transpose <- FALSE
  n.true <- nrow(X)
  p.true <- ncol(X)
  if (nrow(X) < ncol(X)) {
    take.transpose <- TRUE
    X <- t(X)
  }
  
  # ISA is formulated in terms of the X'X
  if(transformation == "no") {
    if(!center){
      Xc <- X
      center.shift <- 0
    } else { 
      if (take.transpose == TRUE) {
       Xc <- t(scale(t(X), scale = F))
      } else {
       Xc <- scale(X, scale = F)
      }
      center.shift <- X - Xc
    }
  } else if (transformation == "ca") {
    R <- rowSums(X)
    C <- colSums(X)
    if (prod(R) * prod(C) == 0) {
      stop("to run CA, each row and columns must have a positive number of counts")
    }
    indep.term <- 1 / sum(X) * matrix(R, length(R), 1) %*% matrix(C, 1, length(C))
    Xc <- diag(1/sqrt(R)) %*% (X - indep.term) %*% diag(1/sqrt(C))
  } else {
    stop("invalid transformation")
  }
  
  M <- t(Xc) %*% Xc
  
  # the matrix S encodes information about the noise model
  if(transformation == "no") {  
    if (noise == "gaussian") {
      S <- diag(nrow(X) * (delta/(1-delta))* sigma^2, ncol(X), ncol(X))
    } else if (noise == "binomial") {
      S <- delta / (1 - delta) * diag(colSums(X))
    } else {
      stop("unknown noise model")
    }
  } else if (transformation == "ca") {
    S <- delta / (1 - delta) * diag(colSums(diag(1/R) %*% X %*% diag(1/C)))
  } else {
    stop("invalid transformation")
  }
  
  # run the iteration
  Mhat <- M
  trace.M <- sum(diag(M))
  
  for(iter in 1:maxiter) {
    Bhat <- solve(Mhat + S, Mhat)
    Mhat.old <- Mhat
    Mhat <- t(Bhat) %*% M %*% Bhat
    
    if (iter > 5 & sum(diag(Mhat.old)) - sum(diag(Mhat)) < threshold * trace.M) {
      break;
    }
    
    if(iter == maxiter) {
      warning(paste("iteration did not converge in", maxiter, "steps"))
    }
  }
  
  # delete the almost-zero singular values of B
  if(svdmethod == "svd"){
    B.svd <- svd(Bhat, nu = nu, nv = nu)
  } else {
    B.svd <- irlba(Bhat, nu = nu, nv = nu)
  }
  
  k <- sum(B.svd$d > svd.cutoff)
  
  # if there are no non-zeros, we can terminate early
  if(k == 0) {
    return(list(mu.hat=matrix(0, n.true, p.true),
                nb.eigen=0,
                low.rank=list(u=0, d=0, v=0),
                nb.iter=iter))
  }
  
  # Compute in low-rank form to avoid taking svd of MUHAT
  # MUHAT = Xc Bhat
  mu.left <-  Xc %*% B.svd$u[, 1:k, drop=FALSE] %*% diag(B.svd$d[1:k],k ,k )
  mu.left <- svd(mu.left)
  mu.svd <- list(u=mu.left$u,
                 d=mu.left$d,
                 v=B.svd$v[, 1:k, drop=FALSE] %*% mu.left$v)
  muhat.centered <- mu.svd$u %*% diag(mu.svd$d, k, k) %*% t(mu.svd$v)
  
  # undo transformations
  if (transformation == "no") {  
    muhat <- muhat.centered + center.shift
  } else if (transformation == "ca") {
    muhat <- diag(sqrt(R)) %*% muhat.centered %*% diag(sqrt(C)) + indep.term
  } else {
    stop("invalid transformation")
  }
  
  if(take.transpose) {
    muhat <- t(muhat)
    tmp <- mu.svd$u
    mu.svd$u <- mu.svd$v
    mu.svd$v <- tmp
  }
  
  return(list(mu.hat=muhat,
              nb.eigen=k,
              low.rank=mu.svd,
              nb.iter=iter))
}
