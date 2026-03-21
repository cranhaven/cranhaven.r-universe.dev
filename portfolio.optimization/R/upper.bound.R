#' @title Set upper bounds on assets
#' 
#' @description
#' \code{upper.bound} sets lower bounds on assets within a portfolio.model
#'
#' @param model the portfolio.model to adapt the upper bounds
#' @param v1 either one upper bound or lower bounds for all assets 
#' @param v2 if not empty then v1 contains the positions (or names) 
#' and v2 the bounds
#' 
#' @return portfolio.model with new upper bounds
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export 
upper.bound <- function(model, v1=NULL, v2=NULL) {
  # if no value is given, print bounds 
  if (is.null(v1)) { print(model$asset.bound.upper) } else {
    if (is.null(v2)) {
      if(length(v1) == 1) {
        # 1. v1 is a numeric scalar
        model$asset.bound.upper <- rep(v1, model$assets)
      } else {
        # 2. v1 is a numeric vector of length(model$assets)
        if (length(v1) == model$assets) { model$asset.bound.upper <- v1 }
      }
    } else {
      # 3. v1 is an index scalar and v2 the bound for asset in v1
      if (is.numeric(v1) & (length(v1) == 1)) {
        if ((v1 >= 1) & (v1 <= model$assets)) { model$asset.bound.upper[v1] <- v2 } 
      }    
      # 4. v1 is a character string and v2 the bound for asset in v1
      if (is.character(v1) & (length(v1) == 1)) {
        p1 <- which(names(model$data) == v1)
        if ((p1 >= 1) & (p1 <= model$assets)) { model$asset.bound.upper[p1] <- v2 } 
      }    
      
      # 5. v1 is an index vector and v2 the bounds for asset in v1, len(v1) == len(v2)
      if (is.numeric(v1) & (length(v1) > 1) & (length(v1) == length(v2))) {
        model$asset.bound.upper[v1] <- v2
      }
      
      # 6. v1 is a character vector and v2 the bounds for asset in v1, len(v1) == len(v2)
      if (is.character(v1) & (length(v1) > 1) & (length(v1) == length(v2))) {
        p1 <- as.numeric(sapply(v1, function(x) { which(names(model$data) == x) }))
        # ! do a sanity check of p1 here
        model$asset.bound.upper[p1] <- v2
      }
    }
  }
  return(model)
}
