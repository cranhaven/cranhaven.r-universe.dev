#' Exact weights from a ps.xgb object
#'
#' Extracts weights from a ps.xgb object, output with the unique identifier for easy merging.
#' 
#' @param x An object of class [ps.xgb]
#' @return Returns a data frame.
#' 
#' @examples
#' # See vignette for examples.
#' 
#' @export

get.weights = function(x){
   if (class(x)=="ps.xgb"){
      return(x$weight.data)
   }else{
      stop("x must be of class ps.xgb")
   }
}