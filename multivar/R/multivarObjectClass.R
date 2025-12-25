# Check multivar object.
check.multivar <- function(object){

    errors <- character()

    if(any(is.na(object@A))){
      msg <- c("multivar error: remove NA values before running constructModel.")
      errors <- c(errors,msg)
    }

    if(length(errors)==0) TRUE else errors

}

#' multivar object class
#'
#' An object class to be used with cv.multivar
#' 
#' @slot k Numeric. The number of subjects (or groupings) in the dataset.
#' @slot n Numeric Vector. Vector containing the number of timepoints for each dataset.
#' @slot d Numeric Vector. Vector containing the number of variables for each dataset.
#' @slot Ak List. A list (length = k) of lagged (T-lag-horizon) by d multivariate time series.
#' @slot bk List. A list (length = k) of (T-lag-horizon) by d multivariate time series.
#' @slot Hk List. A list (length = k) of (horizon) by d multivariate time series.
#' @slot A  Matrix. A matrix containing the lagged ((T-lag-horizon)k) by (d+dk) multivariate time series.
#' @slot b  Matrix. A matrix containing the non-lagged ((T-lag-horizon)k) by (d) multivariate time series.
#' @slot H  Matrix. A matrix containing the non-lagged (horizon k) by d multivariate time series.
#' @slot lag Numeric. The VAR order. Currently only lag 1 is supported.
#' @slot horizon Numeric. Forecast horizon.
#' @slot t1 Numeric vector. Index of time series in which to start cross validation for individual k. 
#' @slot t2 Numeric vector. Index of time series in which to end cross validation for individual k.
#' @slot lambda1 Numeric vector. Regularization parameter 1.
#' @slot lambda2 Numeric vector. Regularization parameter 2.
#' @slot nlambda1 Numeric. Number of lambda1 values to search over. Default is 30.
#' @slot nlambda2 Numeric. Number of lambda2 values to search over. Default is 30.
#' @slot tol Numeric. Convergence tolerance.
#' @slot depth Numeric. Depth of grid construction. Default is 1000.
#' @slot window Numeric. Size of rolling window.
#' @slot standardize Logical. Default is true. Whether to standardize the individual data.
#' @slot weightest Character. How to estimate the first-stage weights. Default is "lasso". Other options include "ridge", "ols" and "var". 
#' @slot canonical Logical. Default is false. If true, individual datasets are fit to a VAR(1) model.
#' @slot threshold Logical. Default is false. If true, and canonical is true, individual transition matrices are thresholded based on significance.
#' @slot lassotype Character. Default is "adaptive". Choices are "standard" or "adaptive" lasso.
#' @slot intercept Logical. Default is FALSE. 
#' @slot W Matrix. Default is NULL. 
#' @slot ratios Numeric vector. Default is NULL. 
#' @slot cv Character. Default is "blocked" for k-folds blocked cross-validation. rolling window cross-validation also available using "rolling".  If "blocked" is selected the nfolds argument should be specified.
#' @slot nfolds Numeric. The number of folds for use with "blocked" cross-validation.
#' @slot thresh Numeric. Post-estimation threshold for setting the individual-level coefficients to zero if their absolute value is smaller than the value provided. Default is zero.
#' @slot lamadapt Logical. Should the lambdas be calculated adaptively. Default is FALSE.
#' @details To construct an object of class multivar, use the function \code{\link{constructModel}}
#' @seealso \code{\link{constructModel}}
#' @export
setClass(
    Class="multivar",
    representation(
        k = "numeric",
        n  = "numeric",
        d  = "numeric",
        Ak = "list",
        bk = "list",
        Hk = "list",
        A  = "matrix",
        b  = "matrix",
        H  = "matrix",
        lag="numeric",
        horizon="numeric",
        t1 = "numeric",
        t2 = "numeric",
        t1k = "numeric",
        t2k = "numeric",
        ntk = "numeric",
        ndk = "numeric",
        lambda1="matrix",
        lambda2="matrix",
        nlambda1="numeric",
        nlambda2="numeric",
        gamma = "numeric",
        tol="numeric",
        depth="numeric",
        window="numeric",
        standardize = "logical",
        weightest = "character",
        canonical = "logical",
        threshold = "logical",
        lassotype = "character",
        intercept = "logical",
        W = "array",
        ratios = "numeric",
        cv = "character",
        nfolds = "numeric",
        thresh = "numeric",
        lamadapt = "logical"
        ),validity=check.multivar
    )


#' Construct an object of class multivar
#' 
#' @param data List. A list (length = k) of T by d multivariate time series
#' @param lag Numeric. The VAR order. Default is 1.
#' @param horizon Numeric. Desired forecast horizon. Default is 1. ZF Note: Should probably be zero.
#' @param t1 Numeric. Index of time series in which to start cross validation. If NULL, default is floor(nrow(n)/3) where nk is the time series length for individual k.
#' @param t2 Numeric. Index of times series in which to end cross validation. If NULL, default is floor(2*nrow(n)/3) where nk is the time series length for individual k.
#' @param lambda1 Matrix. Regularization parameter 1. Default is NULL.
#' @param lambda2 Matrix. Regularization parameter 2. Default is NULL.
#' @param nlambda1 Numeric. Number of lambda1 values to search over. Default is 30.
#' @param nlambda2 Numeric. Number of lambda2 values to search over. Default is 30.
#' @param depth Numeric. Depth of grid construction. Default is 1000.
#' @param tol Numeric. Optimization tolerance (default 1e-4).
#' @param window Numeric. Size of rolling window.   
#' @param standardize Logical. Default is true. Whether to standardize the individual data.
#' @param weightest Character. Default is "mlr" for multiple linear regression. "sls" for simple linear regression also available. How to estimate the first-stage weights.
#' @param canonical Logical. Default is false. If true, individual datasets are fit to a VAR(1) model.
#' @param threshold Logical. Default is false. If true, and canonical is true, individual transition matrices are thresholded based on significance.
#' @param lassotype Character. Default is "adaptive". Choices are "standard" or "adaptive" lasso.
#' @param intercept Logical. Default is FALSE. 
#' @param W Matrix. Default is NULL. 
#' @param ratios Numeric vector. Default is NULL. 
#' @param cv Character. Default is "rolling" for rolling window cross-validation. "blocked" is also available for blocked folds cross-validation. If "blocked" is selected the nfolds argument should bbe specified.
#' @param nfolds Numeric. The number of folds for use with "blocked" cross-validation.
#' @param thresh Numeric. Post-estimation threshold for setting the individual-level coefficients to zero if their absolute value is smaller than the value provided. Default is zero.
#' @param lamadapt Logical. Should the lambdas be calculated adaptively. Default is FALSE.
#' @examples
#' 
#' sim  <- multivar_sim(
#'   k = 2,  # individuals
#'   d = 3,  # number of variables
#'   n = 20, # number of timepoints
#'   prop_fill_com = 0.1, # proportion of paths common
#'   prop_fill_ind = 0.1, # proportion of paths unique
#'   lb = 0.1,  # lower bound on coefficient magnitude
#'   ub = 0.9,  # upper bound on coefficient magnitude
#'   sigma = diag(3) # noise
#' )
#' 
#' plot_sim(sim, plot_type = "common")
#' 
#' model <- constructModel(data = sim$data, weightest = "ols")
#'
#' @export
constructModel <- function( data = NULL,
                            lag = 1,
                            horizon=0,
                            t1 = NULL, 
                            t2 = NULL, 
                            lambda1=NULL,
                            lambda2=NULL,
                            nlambda1=30,
                            nlambda2=30,
                            depth = 1000,
                            tol=1e-4,
                            window = 1,
                            standardize = T,
                            weightest = "lasso",
                            canonical = FALSE,
                            threshold = FALSE,
                            lassotype = "adaptive",
                            intercept = FALSE,
                            W = NULL,
                            ratios = NULL,
                            cv = "blocked",
                            nfolds = 10,
                            thresh = 0,
                            lamadapt = FALSE){
  
  if( lag != 1 ){
    stop("multivar ERROR: Currently only lag of order 1 is supported.")
  }
  
  # if(!is.null(horizon) & horizon<1 ){
  #   stop("Forecast Horizon must be at least 1")
  # }
  
  if( tol<0 | tol>1e-1 ){
    stop("Tolerance must be positive")
  }
  
  dat <- setup_data(data, standardize, lag, horizon) 
  
  getj  <- function(mat){dp = diff(mat@p);rep(seq_along(dp), dp) - 1}
  k     <- length(dat)
  p     <- ncol(dat[[1]]$A)
  ns    <- sapply(dat, function(item){nrow(item$A)})
  cns   <- endrows <- cumsum(ns)
  sr    <- c(1,cns[-length(cns)] + 1) - 1
  nz    <- tail(cns,1)
  is    <- js <- xs <- NULL
  	
  for(ii in 1:k){
  	is <- c(is, sr[ii] + dat[[ii]]$A@i)
  	js <- c(js, getj(dat[[ii]]$A))
  	xs <- c(xs, dat[[ii]]$A@x)
  	is <- c(is, sr[ii] + dat[[ii]]$A@i)
  	js <- c(js, getj(dat[[ii]]$A) + p*ii)
  	xs <- c(xs, dat[[ii]]$A@x)
  }
  
  Ak <- lapply(dat, "[[", "A")
  bk <- lapply(dat, "[[", "b")
  Hk <- lapply(dat, "[[", "H")
  
  if(k == 1) {
     A  <- Matrix(Ak[[1]], sparse = TRUE)
  } else {
     A  <- sparseMatrix(i = is, j = js, x = xs, index1=FALSE, dims = c(nz,p*(k+1)))
  }
  
  b  <- as.matrix(do.call(rbind, bk))
  H  <- as.matrix(do.call(rbind, Hk))
  
  # here we also assume all individuals have the same number
  # of timepoints. (zff 2021-09-15)
  
  #if(is.null(t1)){
    #t1 <- nrow(Ak[[1]]) - floor(.33*(nrow(Ak[[1]]))) 
  #  t1 <- nrow(Ak[[1]]) - floor(.5*(nrow(Ak[[1]]))) 
  #}
  
  #(is.null(t2)){
  #  t2 <- nrow(Ak[[1]])
  #}
  
  # adjust t1 and t2 by max lag to account for initialization
  #t1 <- t1 - lag
  #t2 <- t2 - lag
  
  # what indices do we need for forecasting
  t1k <- unlist(lapply(dat, function(x){floor(nrow(x$b)/3)}))
  #t2k <- unlist(lapply(dat, function(x){floor(2*nrow(x$b)/3)}))
  t2k <- unlist(lapply(dat, function(x){nrow(x$b)}))
  ntk <- unlist(lapply(dat, function(x){nrow(x$b)})) # number tps
  ndk <- unlist(lapply(dat, function(x){ncol(x$b)})) # number cols
  
  # tks <- c(1, cumsum(ntk[-length(ntk)])+1)
  # tke <- cumsum(ntk)
  # t1s <- c(1, cumsum(t1k[-length(t1k)])+1)
  # t1e <- cumsum(t1k)
  # t2s <- t1e+1
  # t1e <- cumsum(t1k)
  
  if(is.null(lambda1) & is.null(lambda2)){
    # construct ratios, and initialize vectors for lambda1, lambda2.
    ratios <- rev(round(exp(seq(log(k/depth),log(k),length.out = nlambda1)), digits = 10))
    lambda1 <- matrix(0, nlambda1,length(ratios))
    lambda2 <- matrix(0, nlambda2,length(ratios))
  } else {
    nlambda1 <- length(lambda1)
    nlambda2 <- length(lambda2)
    lambda1 <- matrix(lambda1, nrow = 1)
    lambda2 <- matrix(lambda2, nrow = 1)
    ratios <- c(0)
  }


  # construct W
  W <- matrix(1, nrow = ncol(bk[[1]]), ncol = ncol(A))
  
  obj <- new("multivar",
    k  = k,
    n  = ntk,
    d  = ndk,
    Ak = Ak,
    bk = bk,
    Hk = Hk,
    A  = as.matrix(A),
    b  = b,
    H  = H,
    horizon = horizon,
    t1 = t1k,
    t2 = t2k,
    t1k = t1k,
    t2k = t2k,
    ntk = ntk,
    ndk = ndk,
    lambda1 = lambda1,
    lambda2 = lambda2,
    nlambda1 = nlambda1,
    nlambda2 = nlambda2,
    depth = depth,
    tol = tol,
    window = window,
    weightest = weightest,
    canonical  = canonical,
    threshold  = threshold,
    lassotype = lassotype,
    intercept = intercept,
    W = W,
    ratios = ratios,
    cv = cv,
    nfolds = nfolds,
    thresh = thresh,
    lamadapt = lamadapt
  )

  return(obj)

}


# show-default method to show an object when its name is printed in the console.
#' Default show method for an object of class multivar
#'
#' @param object \code{multivar} object created from \code{ConstructModel}
#' @return Displays the following information about the multivar object:
#' \itemize{
#' \item{To do.}
#' }
#' @seealso \code{\link{constructModel}} 
#' @name show.multivar
#' @aliases show,multivar-method
#' @docType methods
#' @rdname show-methods
#' @export
setMethod("show","multivar",function(object){
  cat("*** multivar model *** \n")
  cat("Number of groupings: ") ; cat(object@k, "\n")
  cat("Forecast horizon: ") ; cat(object@horizon, "\n")
  cat("Number of variables: \n") ; cat("    ");print(object@d)
  cat("Number of timepoints: \n") ; cat("    ");print(object@n)
})




#' Cross Validation for multivar
#' 
#' @usage cv.multivar(object)
#' @param object multivar object built using \code{ConstructModel}.
#' @details The main function of the multivar package. Performs cross validation to select penalty parameters over a training sample and evaluates them over a test set.
#' @return An object of class \code{multivar.results}.
#' @name cv.multivar
#' @aliases cv.multivar,multivar-method
#' @docType methods
#' @rdname cv.multivar-methods
#' @examples
#' 
#' # example 1 (run)
#' sim1  <- multivar_sim(
#'   k = 2,  # individuals
#'   d = 5,  # number of variables
#'   n = 20, # number of timepoints
#'   prop_fill_com = 0.1, # proportion of paths common
#'   prop_fill_ind = 0.05, # proportion of paths unique
#'   lb = 0.1,  # lower bound on coefficient magnitude
#'   ub = 0.5,  # upper bound on coefficient magnitude
#'   sigma = diag(5) # noise
#' )
#' 
#' model1 <- constructModel(data = sim1$data)
#' fit1 <- multivar::cv.multivar(model1)
#'
#'
#' @export
setGeneric(name = "cv.multivar",def=function(object){standardGeneric("cv.multivar")})
setMethod(f = "cv.multivar", signature = "multivar",definition = function(object){


  # this includes the intercept? do we want this?
  #
  # here we use d[1] and assume all individuals have the same number
  # of predictors. when this is relaxed this should be modified 
  # accordingly. (zff 2021-09-15)
  
  if(object@k == 1){
    B <- array(0,dim = c((object@d[1]),(object@d[1]*(object@k) + 1), object@nlambda1*length(object@ratios)))
  } else {
    B <- array(0,dim = c((object@d[1]),(object@d[1]*(object@k + 1) + 1), object@nlambda1*length(object@ratios)))
  }
  
  
  object@W <- est_base_weight_mat(
    object@W,
    object@Ak,
    object@bk,
    object@ratios, 
    object@d, 
    object@k, 
    object@lassotype, 
    object@weightest
  )
  
 
  object@lambda1 <- lambda_grid(
    B,
    object@depth, 
    object@nlambda1, 
    t(as.matrix(object@b)), 
    t(as.matrix(object@A)), 
    object@W, 
    object@k,
    object@tol,
    object@intercept,
    object@lamadapt
  ) 
  

  
  fit <- cv_multivar(
    B, 
    t(as.matrix(object@A)), 
    t(as.matrix(object@b)), 
    object@W, 
    object@Ak,
    object@bk,
    object@k, 
    object@d, 
    object@lambda1, 
    object@lambda2, 
    object@ratios, 
    object@t1, 
    object@t2, 
    eps = 1e-3,
    object@intercept,
    object@cv,
    object@nfolds
  )
  

  mats <- breakup_transition(
    fit[[1]][,,which.min(colMeans(fit[[2]]))], 
    object@Ak, 
    object@ndk, 
    object@intercept,
    object@thresh
  )
  
  results <- list(
    mats = mats,
    beta = fit[[1]],
    MSFE = fit[[2]],
    obj  = object
  )
  
  #results <- new("multivar.results",object)
  return(results)
})

#' Canonical VAR Fitting Function for multivar
#' 
#' @usage canonical.multivar(object)
#' @param object multivar object built using \code{ConstructModel}.
#' @details A function to fit a canonical VAR model to each individual dataset. 
#' @return A list of results.
#' @seealso \code{\link{constructModel}}, 
#' @name canonical.multivar
#' @aliases canonical.multivar,multivar-method
#' @docType methods
#' @rdname canonical.multivar-methods
#' @examples
#' 
#' # example 1 (run)
#' sim1  <- multivar_sim(
#'   k = 2,  # individuals
#'   d = 5,  # number of variables
#'   n = 20, # number of timepoints
#'   prop_fill_com = 0.1, # proportion of paths common
#'   prop_fill_ind = 0.05, # proportion of paths unique
#'   lb = 0.1,  # lower bound on coefficient magnitude
#'   ub = 0.5,  # upper bound on coefficient magnitude
#'   sigma = diag(5) # noise
#' )
#' 
#' model1 <- constructModel(data = sim1$data, weightest = "ols")
#' fit1 <- canonical.multivar(model1)
#'
#' @export
setGeneric(name = "canonical.multivar",def=function(object){standardGeneric("canonical.multivar")})
setMethod(f = "canonical.multivar", signature = "multivar",definition = function(object){

  can_var <- lapply(seq_along(object@bk), function(i){
    df <- as.matrix(rbind(
      object@Ak[[i]][1,],
      object@bk[[i]]
     ))
     fit_canonical_var(df, p = 1, type = "none")
  })


  res <- list(
    common = NULL,
    unique = NULL,
    total  = lapply(can_var,"[[","transition_mat"),
    total_sigonly  = lapply(can_var,"[[","transition_mat_sigonly")
  )

  return(list(mats = res))
})













