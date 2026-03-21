#' @title Enable active extension portfolios
#' 
#' @description
#' \code{active.extension} adds corresponding long/short constraints for a 
#' diverse set of active extension portfolios (e.g. 130/30 portfolios)
#'
#' @param model the portfolio.model to activate
#' @param up percentage long (e.g. 130) 
#' @param down percentage short (e.g. 30)
#' 
#' @return portfolio.model with active extension enabled
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export 
active.extension <- function(model, up=130, down=30) {
  up <- up/100
  down <- down/100

  model$sum.long <- up # 130/30: 1.3 
  model$sum.short <- down # 130/30: 0.3
  model <- lower.bound(model, -down) # 130/30: -0.3
  model <- upper.bound(model, up) # 130/30: 1.3

  model$active.extension <- TRUE
  
  return(model)
}
