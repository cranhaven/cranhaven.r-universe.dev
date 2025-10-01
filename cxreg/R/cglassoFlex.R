#' fit a complex-valued graphical Lasso for a path of lambda values
#'
#' Fit a complex-valued graphical Lasso formulation for a path of lambda values.
#' \code{cglasso.path} solves the penalized Gaussian maximum likelihood problem for a path of lambda values.
#'
#' @param S p x p-dimensional symmetric spectral density (or spectral coherence) matrix. S is considered as being computed by average smoothed periodogram (the bandwidth is computed by using the given nobs).
#' @param D The p x p-dimensional diagonal matrix with spectral densities as the diagonal entries. Default is \code{NULL}. If D is not provided, diagonals of S are chosen.
#' @param type Logical flag to choose the formulation to solve. Default is \code{I}. If type is \code{I}, the algorithm solves CGLASSO-I in the reference, 
#' \deqn{D^{-1/2}(\arg\min_{\Theta}Tr[\hat{R}\hat{\Theta}]-\log\det\Theta + \sum_{i\neq j}|\Theta_{ij}|)D^{-1/2}} for the given $D$. If type is \code{II}, the algorithm solves CGLASSO-II in the reference. It is for each iterative classo with covariate update, the scale matrix D is multiplied. Please see the reference for the details of the iterative updates.
#' @param nobs Number of observations used in computation of the spectral density matrix S. This quantity is need to compute the Fourier frequency, extended BIC, and bandwidth for the average smoothed periodogram. 
#' @param lambda A user supplied \code{lambda} sequence.
#' Typical usage is to have the program compute its own \code{lambda} sequence based on
#' \code{nlambda} and \code{lambda.min.ratio}.
#' Supplying a value of \code{lambda} overrides this.
#' WARNING: use with care. Avoid supplying a single value for \code{lambda}
#' @param nlambda The number of \code{lambda} values - default is 50.
#' @param lambda.min.ratio Smallest value for \code{lambda}, as a fraction of
#' \code{lambda.max}, the (data derived) entry value (i.e. the smallest value
#' for which all coefficients are zero). The default depends on the sample size
#' \code{nobs} relative to the number of variables \code{nvar}.
#' If \code{nobs > p}, the default is \code{0.0001}, close to zero.
#' If \code{nobs < p}, the default is \code{0.01}.
#' @param W.init Logical flag whether the initially estimated spectral density matrix is given. Default is \code{NULL}.
#' @param stopping_rule Logical flag if the algorithm is terminated by stopping rule. If the algorithm is early terminated, not all estimates for initially designated lambdas are explored. 
#' @param stop_criterion Stopping criterion for early termination. Default is \code{EBIC} (Extended BIC). Alternatively, \code{AIC} (AIC) and \code{RMSE} (root mean squared error between two consecutive estimates) can be used.
#' @param maxit Maximum number of iterations of both outer and inner loops. Default 500.
#' @param thresh Convergence threshold for coordinate descent. Default is 1e-4.
#' @param trace.it If \code{trace.it=1}, then a progress bar is displayed;
#' useful for big models that take a long time to fit.
#' @param \dots Other arguments that can be passed to \code{cglasso}
#' 
#' @return An object with class "cglassofit" and "cglasso".
#' \item{a0}{Intercept sequence of length \code{length(lambda)}.}
#' \item{beta}{A \code{nvar x length(lambda)} matrix of coefficients, stored in
#' sparse matrix format.}
#' \item{df}{The number of nonzero coefficients for each value of lambda.}
#' \item{dim}{Dimension of coefficient matrix.}
#' \item{lambda}{The actual sequence of lambda values used. When alpha=0, the
#' largest lambda reported does not quite give the zero coefficients reported
#' (lambda=inf would in principle). Instead, the largest lambda for alpha=0.001
#' is used, and the sequence of lambda values is derived from this.}
#' \item{dev}{The fraction of (null) deviance explained. The deviance
#' calculations incorporate weights if present in the model. The deviance is
#' defined to be 2*(loglike_sat - loglike), where loglike_sat is the log-likelihood
#' for the saturated model (a model with a free parameter per observation).
#' Hence dev=1-dev/nulldev.}
#' \item{nulldev}{Null deviance (per observation). This is defined to be
#' 2*(loglike_sat -loglike(Null)). The null model refers to the intercept model.}
#' \item{npasses}{Total passes over the data summed over all lambda values.}
#' \item{jerr}{Error flag, for warnings and errors (largely for internal
#' debugging).}
#' \item{call}{The call that produced this object.}
#' \item{family}{Family used for the model.}
#' \item{nobs}{Number of observations.}
#' 
#' #' @return An object with class "cglassofit" and "cglasso".
#' \item{stop_arr}{Sequence of values of information criterion for a fixed lambda.}
#' \item{stop_criterion}{Stopping criterion used.}
#' \item{min_index}{The index for lambda that minimizes the value of the information criterion.}
#' \item{lambda_grid}{Sequence of lambdas used.}
#' \item{Theta_hat}{Estimated inverse spectral matrix for each fixed lambda. It is provided in the list.}
#' \item{type}{Type of the formulation used, either CGALSSO-I or CGLASSO-II.}
#' \item{scale}{Whether the spectral density matrix (covariance) or spectral coherence (coherence) is given.}
#' \item{D}{Used scale diagonal matrix.}
#'
#' @importFrom stats mvfft
#' @useDynLib cxreg cglassocd_noscale cglassocd_scaled
cglasso.path <- function(S,
                         D,
                         type,
                         nobs,
                         lambda,
                         nlambda,
                         lambda.min.ratio,
                         W.init,
                         stopping_rule,
                         stop_criterion,
                         maxit = maxit,
                         thresh = thresh,
                         trace.it = trace.it,...){
  
  ####################################################################
  # Prepare all the generic arguments
  this.call <- match.call()
  
  ####################################################################
  p <- dim(S)[1]
  m <- floor(nobs)
  bandwidth <- 2*m+1
  
  if (is.null(D)){
    D <- diag(diag(S),p)
  }

  ####################################################################
  # work out lambda values
  nlam <- as.integer(nlambda)
  user_lambda <- FALSE   # did user provide their own lambda values?
  if (is.null(lambda)) {
    if (lambda.min.ratio >= 1) {
      stop("lambda.min.ratio should be less than 1")
    }
    lambda_max <- max(max(Mod(S)),sqrt(log(p)/bandwidth))

    # compute lambda sequence
    ulam <- exp(seq(log(lambda_max), log(lambda_max*lambda.min.ratio),
                    length.out = nlam))
  }else {  # user provided lambda values
    user_lambda <- TRUE
    if (any(lambda < 0)) {
      stop("lambdas should be non-negative")
    }
    ulam <- as.double(rev(sort(lambda)))
    nlam <- as.integer(length(lambda))
  }
  stepnames <- paste0("s", 0:(length(ulam) - 1))
  
  # start progress bar
  if (trace.it == 1) {
    pb <- utils::txtProgressBar(min = 0, max = nlam, style = 3)
  }
  
  ####################################################################
  Theta_list <- list()
  W_list <- list()
  stop_arr <- rep(NA,nlam)
  if (type=="I"){
    S_sc <- D %*% S %*% D
  }
  
  ####################################################################
  # get feature variable names
  vnames <- colnames(S)
  if(is.null(vnames)){
    vnames <- paste("V",seq(p),sep="") 
  }
  
  ####################################################################
  # Starting value of the best index
  i_best <- 0
  flag <- 0
  count_na <- 0
  count_ex <- 0
  for(i in 1:nlam){
    lambda <- ulam[i]
    
    if (trace.it == 2) {
      cat("Fitting lambda index", i, ":", ulam[i], fill = TRUE)
    }
    
    if(i == 1){
      if (type=="I"){
        out <- .Fortran("cglassocd_noscale",
                        s = as.complex(S_sc),
                        p = as.integer(p),
                        lambda = as.double(lambda),
                        theta = array(as.complex(0), c(p,p)),
                        w = array(as.complex(0), c(p,p)),
                        w0 = as.complex(S_sc),
                        w_init = as.logical(FALSE),
                        maxiter = as.integer(maxit),
                        tol = as.double(thresh),
                        h = as.integer(0),
                        final_cycle = as.logical(FALSE)
        )
        Theta_hat <- D %*% out$theta %*% D
        
      }else{
        
        out <- .Fortran("cglassocd_scaled",
                        s = as.complex(S),
                        p = as.integer(p),
                        lambda = as.double(lambda),
                        theta = array(as.complex(0), c(p,p)),
                        w = array(as.complex(0), c(p,p)),
                        w0 = as.complex(S),
                        w_init = as.logical(FALSE),
                        maxiter = as.integer(maxit),
                        tol = as.double(thresh),
                        h = as.integer(0),
                        final_cycle = as.logical(FALSE)
        )
        Theta_hat <- out$theta
        
      }
      W_list[[i]] <- out$w
      W_init <- out$w
      
    } else {
      # For other lambda, there is a warm start
      if (type=="I") {
        out <- .Fortran("cglassocd_noscale",
                        s = as.complex(S_sc),
                        p = as.integer(p),
                        lambda = as.double(lambda),
                        theta = array(as.complex(0), c(p,p)),
                        w = array(as.complex(0), c(p,p)),
                        w0 = as.complex(W_init),
                        w_init = as.logical(TRUE),
                        maxiter = as.integer(maxit),
                        tol = as.double(thresh),
                        h = as.integer(0),
                        final_cycle = as.logical(FALSE)
        )
        Theta_hat <- D %*% out$theta %*% D
        
      }else {
        out <- .Fortran("cglassocd_scaled",
                        s = as.complex(S),
                        p = as.integer(p),
                        lambda = as.double(lambda),
                        theta = array(as.complex(0), c(p,p)),
                        w = array(as.complex(0), c(p,p)),
                        w0 = as.complex(W_init),
                        w_init = as.logical(TRUE),
                        maxiter = as.integer(maxit),
                        tol = as.double(thresh),
                        h = as.integer(0),
                        final_cycle = as.logical(FALSE)
        )
        Theta_hat <- out$theta
        
      }
      
      if(any(Mod(out$w) > 0) && !any(is.nan(Mod(out$w)))){
        W_list[[i]] <- out$w
        W_init <- out$w
      }else{
        W_list[[i]] <- NULL
      }
    }
    ####################################################################
    if (trace.it == 1) {
      utils::setTxtProgressBar(pb, i)
    }
    
    
    rownames(Theta_hat) <- vnames
    colnames(Theta_hat) <- vnames
    Theta_list[[i]] <- Theta_hat
    if (stop_criterion == "EBIC"){
      stop_arr[i] <- EBIC_G_trimmed(S, Theta_hat, m, g = 0.5)
    }else if(stop_criterion == "AIC"){
      stop_arr[i] <- AIC(S, Theta_hat, m)
    }else if (stop_criterion == "RMSE"){
      if (i == 1){
        stop_arr[i] <- 0
      }else{
        stop_arr[i] <- rel.diff(Theta_hat, Theta_list[[(i-1)]])
      }
    }
    
    ####################################################################
    # Stopping rule for the given criterion
    c_val <- stop_arr[i]
    if(stopping_rule){
      if(i == 1){
        Theta_hat_best <- Theta_hat
        start_c <- c_val
        min_c <- c_val
        i_best <- i
      }
      
      if(!is.na(c_val)){
        now_c <- c_val
        min_c <- min(min_c, now_c)
        if(min_c >= now_c){
          i_best <- i
          Theta_hat_best <- Theta_hat
        }
        
        if(start_c > min_c && now_c >= min_c * 1.1 && now_c - min_c > 0.4 * (start_c - min_c)){
          count_ex <- count_ex + 1
        } else if(start_c == min_c && now_c - start_c > abs(start_c) * 1.5){
          count_ex <- count_ex + 1
        }
        
      }else {
        count_na <- count_na + 1
      }
      
      if(max(count_ex, count_na) > 5){
        names(Theta_list) <- stepnames[1:i]
        ulam <- ulam[1:i]
        stop_arr <- stop_arr[1:i]
        print(paste("The algorithm was terminated at",i,"th lambda"))
        break
      }
    }
    
    if(trace.it){
      print(paste("end of", i, "-th value, log2(lambda) = ", log2(lambda)))
    }
  }
  
  if (trace.it == 1) {
    utils::setTxtProgressBar(pb, nlam)
    cat("", fill = TRUE)
  }
  
  if(!stopping_rule){
    i_best <- tail(which(stop_arr == min(stop_arr, na.rm = TRUE)), 1)
  }
  
  ####################################################################
  # output
  output <- list(stop_arr = stop_arr,
                 stop_criterion = stop_criterion,
                 min_index = i_best,
                 lambda_grid = ulam,
                 Theta_list = Theta_list,
                 type = type,
                 D = D)
  class(output) <- c("cglassofit","cglasso")
  
  return(output)
}

####################################################################
#' Discrete Fourier Transform of matrix X
#' 
#' Computes the (normalized) discrete Fourier transform (DFT) of a matrix \code{X} row-wise using \code{mvfft}, and extracts a window of frequencies centered at a target index.
#'
#' @param X A numeric matrix of size \eqn{nobs \times nvar}, where DFT is applied across the rows (time points).
#' @param j An integer index in \eqn{1,\ldots,nobs} around which the frequency window is centered.
#' @param m A non-negative integer specifying the window half-width. The function returns \code{2m + 1} DFT components centered around \code{j}.
#'
#' @return A complex-valued matrix of dimension \code{(2m + 1) × nvar} representing selected DFT components of the original matrix.
#'
#' @export
dft.X <- function(X, j, m){
  n = nrow(X); p = ncol(X)
  dft <- mvfft(X)/sqrt(2*pi*n)
  # dft <- mvfft(X)/sqrt(n)
  vj <- j + c(-m:m)
  ind <- fixm(vj, n)
  Z <- dft[ind, ]
  return(Z)
}

####################################################################
#' Fixes indices that fall outside the valid range \code{1:n} using circular (modulo) wrapping.
#'
#' @param v An integer vector of indices.
#' @param n A positive integer giving the length of the domain; valid indices are \code{1} to \code{nobs}.
#'
#' @return An integer vector of the same length as \code{v}, with all values wrapped into the range \code{1} to \code{n}.
#'
#' @export
fixm <- function(v, n){
  v[v<1] = v[v<1]+n
  v[v>n] = v[v>n]-n
  return(v)
}

####################################################################
rel.diff <- function(Est, Est_prev){
  return(sum(Mod(Est - Est_prev)^2)/sum(Mod(Est_prev)^2))
}

####################################################################
AIC <- function(S, Theta_hat, m){
  p <- nrow(Theta_hat)
  k <- sum(!!upperTriangle(Theta_hat, diag = FALSE))
  log_det_Theta <- sum(log(eigen(Theta_hat)$values))  # prod(eigen(Theta.hat)$values)
  log_lik = (2*m+1)*log_det_Theta/2 -
    sum(diag(Theta_hat%*%S))*(2*m+1)/2
  return(Re(-2*log_lik) + 2*k)
}

####################################################################
EBIC_G_trimmed <- function(S, Theta_hat, m, g = 0.5){
  
  p <- nrow(Theta_hat)
  k <- sum(!!upperTriangle(Theta_hat, diag = FALSE))
  eigen_Theta_hat <- eigen(Theta_hat)
  v <- Re(eigen_Theta_hat$values)
  v[v<=0] <- 1e-6
  P <- eigen_Theta_hat$vectors
  # v[v<=0] = min(v[v>0])
  Theta_hat_corrected <- P %*% diag(v) %*% Conj(t(P))
  log_det_Theta <- sum(log(v)) 
  trace_s_theta <- sum(diag(Theta_hat_corrected %*% S))
  
  ebic <- Re(-(2*m+1)*(log_det_Theta - trace_s_theta)) +  k*log(2*m+1)+4*k*log(p)*g
  
  return(ebic)
}
