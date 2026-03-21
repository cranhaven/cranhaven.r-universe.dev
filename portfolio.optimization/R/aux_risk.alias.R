#' @title Convert risk alias names to internal names
#' 
#' @description
#' \code{aux_risk.alias} converts risk alias names to internal names
#'
#' @param risk the risk name to be standardized
#' 
#' @return the standardized risk name (if any)
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
aux_risk.alias <- function(risk) {
  # alias handling - markowitz
  if (risk == "sd") { risk <- "markowitz" }
  if (risk == "standard.deviation") { risk <- "markowitz" }
  if (risk == "variance") { risk <- "markowitz" }
  
  # alias handling - expected shortfall
  if (risk == "es") { risk <- "expected.shortfall" }
  if (risk == "cvar") { risk <- "expected.shortfall" }
  if (risk == "avar") { risk <- "expected.shortfall" }
  
  return(risk)
}
