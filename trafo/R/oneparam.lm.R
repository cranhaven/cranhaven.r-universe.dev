#' One parameter transformations for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' one parameter transformations. The transformation parameter can either be 
#' estimated using different estimation methods or given. 
#'
#' @param object an object of type lm. 
#' @param trafo character that determines the selected transformation.
#' @param lambda either a character named "estim" if the optimal transformation
#' parameter should be estimated or a numeric value determining a given 
#' transformation parameter. Defaults to "estim".
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Maximum likelihood approach ("ml"), (ii) Skewness minimization ("skew"),
#' (iii) Kurtosis optimization ("kurt"), (iv) Divergence minimization by 
#' Kolmogorov-Smirnov ("div.ks"), by Cramer-von-Mises ("div.cvm") or by 
#' Kullback-Leibler ("div.kl"). Defaults to "ml".
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter. 
#' Defaults to \code{c(-2, 2)}.
#' @param plotit logical. If \code{TRUE}, a plot that illustrates the optimal 
#' transformation parameter or the given transformation parameter is returned.
#' @param custom_trafo a list that determines a one parameter transformation and
#' the standardized one parameter transformation.
#' @param ... other parameters that can be passed to the function.
#' @return an object of class \code{trafo}.
#' @keywords internal

oneparam.lm <- function(object, trafo, lambda = "estim", method = "ml", 
                          lambdarange, plotit = TRUE, custom_trafo = NULL, ...) {
  

  if (is.null(lambdarange) && trafo %in% c("boxcox", "boxcoxshift", "manly", 
                                           "modulus", "yeojohnson", "gpower")) {
    lambdarange <- c(-2,2)
  } else if (is.null(lambdarange) && trafo == "bickeldoksum") {
    lambdarange <- c(1e-11, 2)
  } else if (is.null(lambdarange) && trafo == "dual") {
    lambdarange <- c(0,2)
  } else if (is.null(lambdarange) && trafo %in% c("logshiftopt", "sqrtshift")) {
    span <- range(object$model[, paste0(formula(object)[2])])
    if ((span[1] + 1) <= 1) {
      lower = abs(span[1]) + 1
    } else {
      lower <- -span[1] + 1
    }
    upper <- diff(span)
    
    lambdarange <- c(lower,upper)
  }

 
  check_oneparam(trafo = trafo, lambda = lambda, method = method, 
                 lambdarange = lambdarange, plotit = plotit, 
                 custom_trafo = custom_trafo)
  
  # Get model variables: dependent variable y and explanatory variables x
  model_frame <- object$model 
  
  # Check if arguments are as expected (for model variables)
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")


  if (trafo == "custom") {
    custom_func <- custom_trafo[[1]]
    custom_func_std <- custom_trafo[[2]]
    custom_family <- names(custom_trafo)[[1]]
  }
  
  # For saving returns
  ans <- list()
  
  # Get the optimal transformation parameter
  if (lambda == "estim") {
    optim <- est_lm(y = y, x = x, trafo = trafo, method = method, 
                    lambdarange = lambdarange, 
                    custom_func = custom_func, 
                    custom_func_std = custom_func_std) 
    
    lambdaoptim <- optim$lambdaoptim
    measoptim <- optim$measoptim
    
  } else if (is.numeric(lambda)) {
    lambdaoptim <- lambda
    measoptim <- estim_lm(lambda = lambdaoptim, y = y, x = x, 
                          trafo = trafo, method = method)
  }
  
  # Plot the curve of the measure with line at the optimal transformation 
  # parameter
  if (plotit == TRUE) {
    plot_meas <- plot_trafolm(lambdarange = lambdarange, lambdaoptim = lambdaoptim, 
                              measoptim = measoptim, y = y, x = x, 
                              trafo = trafo, method = method, 
                              custom_func = custom_func, 
                              custom_func_std = custom_func_std)
    
    # Get plot measures
    ans$lambdavector <- plot_meas$lambdavector
    ans$measvector <- plot_meas$measvector
  } else if (plotit == FALSE) {
    ans$lambdavector <- NULL
    ans$measvector <- NULL
  }
  
  # Get vector of transformed and standardized transformed variable
  #ans$yt <- Yeo_john(y = y, lambda = lambdaoptim)
  #ans$zt <- Yeo_john_std(y = y, lambda = lambdaoptim)
  
  # Save transformation family and method
  #ans$family <- "Yeo-Johnson"
  
  ans <- get_transformed(trafo = trafo, ans = ans, y = y, lambda = lambdaoptim,
                         custom_func = custom_func,
                         custom_func_std = custom_func_std, 
                         custom_family = custom_family)
  
  ans$method <- method
  
  ans$lambdahat <- lambdaoptim
  ans$measoptim <- measoptim
  
  # Get transformed model
  #ans$modelt <- get_modelt(object = object, trans_mod = ans, std = FALSE)
  ans$object <- object
  
  # New class trafo
  class(ans) <- c("trafo", "oneparam")
  ans
}