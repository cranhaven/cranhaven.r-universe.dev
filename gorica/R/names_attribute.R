#' @method names model_estimates
#' @export
names.model_estimates <- function(x){
  names(x$estimate)
}


#' @method names<- model_estimates
#' @export
`names<-.model_estimates` <- function(x, value){
  if(!(inherits(x$estimate, "numeric") & (inherits(x$Sigma, "list")|inherits(x$Sigma, "array")))){
    stop("Cannot rename object; might not be a valid model_estimates object.")
  }
  names(x$estimate) <- value
  if(inherits(x$Sigma, "list")){
    x$Sigma <- lapply(x$Sigma, function(s){
      rownames(s) <- colnames(x) <- value
      s
    })
  }
  if(inherits(x$Sigma, "array")){
    rownames(x$Sigma) <- colnames(x$Sigma) <- value
  }
  x
}
