#' Transformations without parameter for linear models
#'
#' The function transforms the dependent variable of a linear model using 
#' different transformations without parameter. 
#'
#' @param object an object of type lm. 
#' @param trafo character that determines the transformation.
#' @param custom_trafo a function that specifies a transformation without 
#' parameter that needs to be estimated or given.
#' @param ... other parameters that can be passed to the function.
#' @return an object of class \code{trafo}.
#' @keywords internal


woparam.lm <- function(object, trafo, custom_trafo = NULL, ...) {
  
  check_woparam(trafo = trafo, custom_trafo = custom_trafo)
  
  model_frame <- object$model 
  
  # Check if arguments are as expected (for model variables)
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  
  # For saving returns
  ans <- list()
  
  
  if (trafo == "log") {
    ans$yt <- Log(y = y)
    ans$zt <- Log_std(y = y)
    ans$family <- "Log"
  } else if (trafo == "logshift") {
    ans$yt <- Log_shift(y = y)$y
    ans$zt <- Log_shift_std(y = y)
    ans$family <- "Log shift"
  } else if (trafo == "reciprocal") {
    ans$yt <- Reciprocal(y = y)
    ans$zt <- Reciprocal_std(y = y)
    ans$family <- "Reciprocal"
  } else if (trafo == "neglog") {
    ans$yt <- neg_log(y = y)
    ans$zt <- neg_log_std(y = y)
    ans$family <- "Neglog"
  } else if (trafo == "glog") {
    ans$yt <- g_log(y = y)
    ans$zt <- g_log_std(y = y)
    ans$family <- "Glog"
  } else if (trafo == "custom") {
    custom_func <- custom_trafo[[1]]
    custom_func_std <- custom_trafo[[1]]
    ans$yt <- custom_func(y = y)
    ans$zt <- custom_func_std(y = y)
    ans$family <- names(custom_trafo)
  }
  
  ans$lambdavector <- NULL
  ans$measvector <- NULL   
  ans$method <- NULL      
  ans$lambdahat <- NULL 
  ans$measoptim <- NULL    
  
  # Save estimation method
  ans$method <- NULL
  
  # Save optimal transformation parameter and corresponding statistics depending
  # on the estimation method
  ans$lambdahat <- NULL
  ans$measoptim <- NULL
  
  # Get transformed model
  #ans$modelt <- get_modelt(object = object, trans_mod = ans, std = FALSE)
  ans$object <- object
  
  # New class trafo
  class(ans) <- c("trafo", "woparam")
  ans
  
}