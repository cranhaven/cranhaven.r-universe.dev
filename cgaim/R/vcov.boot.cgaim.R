#' @rdname vcov.cgaim
#' @order 2
#' 
#' @export
vcov.boot.cgaim <- function(object, parm = c("alpha", "beta"), complete = TRUE, 
  ...)
{
  #----- Header
  
  # Check parm
  parm <- match.arg(parm)
  
  #----- Create variance covariane matrix
  
  # Compute vcov
  vres <- stats::var(t(object$boot[[parm]]))
    
  # Set names
  rownames(vres) <- colnames(vres) <- names(unlist(object$obs[[parm]]))
 
  #----- Complete for potentially aliased coefficients
  if (isTRUE(complete)){
    
    # Check if any aliased coefficients
    aliased <- is.na(unlist(object$obs[[parm]]))
    
    # Complete
    vresa <- matrix(NA, length(aliased), length(aliased),
      dimnames = list(names(unlist(object$obs[[parm]])), 
        names(unlist(object$obs[[parm]]))))
    vresa[!aliased, !aliased] <- vres
  
    #----- Return 
    return(vresa)
  } else {
    return(vres)  
  }
  
}