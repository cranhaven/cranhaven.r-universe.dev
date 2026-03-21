#' @title Return the portfolio weights of a portfolio.model
#' 
#' @description
#' \code{portfolio.weights} return the portfolio weights of a portfolio.model
#'
#' @param model the portfolio.model to return the portfolio weights from
#' 
#' @return a vector of portfolio weights or NULL if no weights are available yet.
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
#' 
#' @examples
#' data(sp100w17av30s)
#' portfolio.weights(optimal.portfolio(scenario.set))
#' 
portfolio.weights <- function(model) {
  if (is.null(model$portfolio)) { return(NULL) } 
  else { return(model$portfolio$x) }
}

#' @rdname portfolio.weights
#' @export
portfolio <- portfolio.weights

#' @rdname portfolio.weights
#' @export
w <- portfolio.weights

#' @rdname portfolio.weights
#' @export
weights <- portfolio.weights

#' @rdname portfolio.weights
#' @export
x <- portfolio.weights
