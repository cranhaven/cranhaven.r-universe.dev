
#' Personalised model
#'
#' Compute personalised models from cforest object.
#'
#' @param x cforest object or matrix of weights.
#' @param model model object. If NULL the model in \code{x$info$model} is used.
#' @param newdata new data. If NULL cforest learning data is used. Ignored if \code{x} is a matrix.
#' @param OOB In case of using the learning data, should patient similarities be
#' computed out of bag?
#' @param fun function to apply on the personalised model before returning. The
#' default \code{coef} returns a matrix of personalised coefficients. For returning
#' the model objects use \code{identity}.
#' @param return_attr which attributes to add to the object returned. If it contains
#' \code{"modelcall"} the call of the base model is returned, if it contains 
#' \code{"data"} the data, and if it contains \code{"similarity"} the matrix of 
#' similarity weights is added.
#'
#' @return depends on fun.
#' 
#' @example inst/examples/ex-pmodel.R
#' 
#' 
#' @export
#' @importFrom methods is
pmodel <- function(x = NULL, model = NULL, newdata = NULL, OOB = TRUE, fun = coef,
                   return_attr = c("modelcall", "data", "similarity")) {
  
  ## compute similarity weights
  if(is(x, "matrix")) {
    if(is.null(model)) stop("When x is a matrix, model must not be NULL. Please enter a model object.")
    pweights <- x
  } else {
    if(is.null(model)) model <- x$info$model
    pweights <- predict(x, type = "weights", newdata = newdata, OOB = OOB)
  }
  
  ## personalised model or model coefficients
  get_pmod <- function(w) {
    
    if (sum(w) == 0) {
        warning("The weights for one observation are all 0, return base coefficients. 
                 A solution may be increasing ntree.")
        return(coef(model))
    }
    dat <- x$data
    dat$w <- w
    
    ## compute the model
    pmod <- update(model, weights = w, subset = w > 0, data = dat)
    
    ## return model or coefficients
    fun(pmod)
  }
  ret <- apply(pweights, 2, get_pmod)

  if(is(ret, "matrix")) ret <- t(ret)
  if(all.equal(fun, identity, check.environment = FALSE) == TRUE) class(ret) <- c("pmodel_identity", class(ret))
  class(ret) <- c("pmodel", class(ret))
  
  if("modelcall" %in% return_attr) attr(ret, "modelcall") <- getCall(model)
  if("data" %in% return_attr) 
    if(is.null(newdata))  attr(ret, "data") <- x$data else  
      attr(ret, "data") <- newdata
  if("similarity" %in% return_attr) attr(ret, "similarity") <- pweights
  
  return(ret)
}
