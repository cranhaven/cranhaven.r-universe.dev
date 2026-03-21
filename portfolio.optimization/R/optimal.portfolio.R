#' @title Meta-function to optimize portfolios given a portfolio.model instance
#'
#' @description
#' \code{optimal.portfolio} optimizes the portfolio of a model given the current
#' specification
#'
#' @param input either a portfolio.model or something to convert to a new model
#' @param ... other parameters to be passed on to the optimization sub-functions.
#' 
#' @return an S3 object of class portfolio.model with the optimized portfolio.
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
#' 
#' @examples
#' data(sp100w17av30s)
#' model <- optimal.portfolio(scenario.set)
#'
optimal.portfolio <- function(input=NULL, ...) {

  ### 1. Check the input
  
  # if not the input is not a portfolio.model, create a new model
  if('portfolio.model' %in% class(input)) {
    model <- input
  } else {
    # if it is not a portfolio.model create a new one
    model <- portfolio.model(input, ...)
  }

  # if the model is not a portfolio.model after step one, exit
  if(!('portfolio.model' %in% class(model))) {
    stop("Unable to create a portfolio.model with the given input!")
  }
  
  ### 2. Select appropriate optimization function (based on parameter set)

  # # Objective Function: reward
  # if (model$objective == "reward") {
  #   model <- optimal.portfolio.reward(model, ...) 
  # }
  
  # Objective Function: 1overN
  if (model$objective == "1overN") {
    model <- optimal.portfolio.1overN(model, ...)
  }
   
  # # Objective Function: momentum
  # if (model$objective == "momentum") {
  #   model <- optimal.portfolio.momentum(model, ...)
  # }

  # Objective Function: markowitz
  if (model$objective == "markowitz") {
    model <- optimal.portfolio.markowitz(model, ...)
  }

  # Objective Function: mad
  if (model$objective == "mad") {
    if (model$active.extension) {
      model <- optimal.portfolio.mad.long.short(model, ...)
    } else {
      model <- optimal.portfolio.mad(model, ...)
    }
  }
  
  # Objective Function: expected.shortfall
  if (model$objective == "expected.shortfall") {
    if (model$active.extension) {
      model <- optimal.portfolio.expected.shortfall.long.short(model, ...)
    } else {
      model <- optimal.portfolio.expected.shortfall(model, ...)
    }
  }
  
  # return model
  return(model)
}

#' @rdname optimal.portfolio
#' @export
p.opt <- optimal.portfolio

#' @rdname optimal.portfolio
#' @export
opt.p <- optimal.portfolio
