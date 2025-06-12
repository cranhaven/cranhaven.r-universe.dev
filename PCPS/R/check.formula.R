#' @title Internal function
#' 
#' @description Internal function to check the validity of left hand side in a formula object. 
#' 
#' @encoding UTF-8
#' @param formula An object of class \code{\link{formula}}. 
#' @param vectornames A vector with names to check the index.
#' @return The index of left side of the formula in vectornames.
#' @export
check.formula <- function(formula, vectornames){
  formula <- as.character(as.expression(formula))
  if(length(gregexpr(" ~ ", formula)[[1]][1])!=1){
    stop("Only one ~ operator is allowed in formula")
  }
  formula <- unlist(strsplit(formula, split = " ~ "))[1]
  if(is.na(match(formula, vectornames))){
    stop(paste(formula, "not found in vectornames"))
  }
  reswhich <- which(vectornames %in% formula)
  return(reswhich)
}
