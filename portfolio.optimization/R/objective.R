#' @title Set new objective of a portfolio.model
#' 
#' @description
#' \code{objective} sets a new objective for VaR and Expected Shortfall
#'
#' @param model the portfolio.model to be changed
#' @param objective the new objective
#' 
#' @return the adapted portfolio.model
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
#'
#' @examples
#' data(sp100w17av30s)
#' model <- portfolio.model(scenario.set)
#' mad <- optimal.portfolio(objective(model, "mad"))
#'
objective <- function(model, objective="markowitz") {
  
  # check if alias is used for objective
  objective <- aux_risk.alias(objective)
  
  # check if selected objective is supported
  supported_objectives <- list("1overN", "momentum", "reward",
                               "markowitz", "mad", "expected.shortfall")

  # if not, fall back to markowitz
  if (!(objective %in% supported_objectives)) {
    warning("Selected objective is not supported! Falling back to objective: markowitz")
    objective <- "markowitz"
  }

  # set objective and return model
  model$objective <- objective
  return(model)
}
