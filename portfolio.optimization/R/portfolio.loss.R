#' @title Return the loss distribution of the portfolio.model
#' 
#' @description
#' \code{portfolio.loss} return the loss distribution of the portfolio.model
#'
#' @param model the portfolio.model to display
#' 
#' @return nothing
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
portfolio.loss <- function(model) {
  if(is.na(model$portfolio)) { return(NA) }
  return(as.vector(model$portfolio$x %*% t(model$data)))
}

#' @rdname portfolio.loss
#' @export
l <- portfolio.loss
