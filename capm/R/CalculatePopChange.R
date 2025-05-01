#' Population change.
#' @description Calculate the change in population size between two times. When only one time is specified, the population size at that time is returned.
#' @param model.out output from one of the following functions or a \code{\link{list}} with equivalent structure: \code{\link{SolveIASA}}, \code{\link{SolveSI}},  \code{\link{SolveTC}} or \code{\link{CalculateGlobalSens}}. When the last function is used, its \code{all} argument must be \code{TRUE}.
#' @param variable string with the name of the the output variable for which the change are to be calculated (see the variable argument for \code{\link{PlotModels}}.
#' @param t1 value specifying the first time.
#' @param t2 value specifying the second time.
#' @param ratio logical. When \code{TRUE}, the calculated change is based on poulation size at t2 divided by population size at t1. When \code{FALSE}, the calculated change is based on poulation size at t2 minus population size at t1.
#' @return Value representing the ratio (if \code{ratio} is \code{TRUE}) or the difference (if \code{ratio} is \code{FALSE}) between population size at time t2 and t1. If only one time is specified, the value is the population size at that time.
#' @references Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @export
#' @examples
#' ## IASA model
#' 
#' ## Parameters and intial conditions.
#' data(dogs)
#' dogs_iasa <- GetDataIASA(dogs,
#'                          destination.label = "Pinhais",
#'                          total.estimate = 50444)
#' 
#' # Solve for point estimates.
#' solve_iasa_pt <- SolveIASA(pars = dogs_iasa$pars,
#'                           init = dogs_iasa$init,
#'                           time = 0:15,
#'                           alpha.owned = TRUE,
#'                           method = 'rk4')
#' 
#' # Calculate the population change (ratio) between times 0 and 15.
#' CalculatePopChange(solve_iasa_pt, variable = 'N1', t2 = 15, t1 = 0)
#' 
#' # Calculate the population change (difference) between times 0 and 15.
#' CalculatePopChange(solve_iasa_pt, variable = 'N1', t2 = 15,
#'                    t1 = 0, ratio = FALSE)
#' 
#' # Calculate the population zises at time 15.
#' CalculatePopChange(solve_iasa_pt, variable = 'N1', t2 = 15)
#' 
CalculatePopChange <- function(model.out = NULL, variable = NULL, t1 = NULL, t2 = NULL, ratio = TRUE) {
  if (is.null(t1) & !is.null(t2)) {
    return(model.out$results[model.out$results$time == t2, c('time', variable)])
  }
  if (!is.null(t1) & is.null(t2)) {
    return(model.out$results[model.out$results$time == t1, c('time', variable)])
  }
  if (!is.null(t1) & !is.null(t2)) {
    
    if (class(model.out) == "summary.sensRange") {
      t.2 <- model.out[model.out$x == t2, c("q05", "Mean", "q95")]
      t.1 <- model.out[model.out$x == t1, "Mean"]
      if (ratio) {
        change <- round((t.2 / t.1), 2)
        return(noquote(paste0('At t2, ', variable, ' will be ', change[2],
                              " (", change[1], " - ", change[3],
                              ") times ", variable, ' at t1.')))
      } else {
        change <- abs(round(t.2 - t.1, 2))
        net.change <- "decreased"
        if (t.2 - t.1 > 0) {net.change <- "increased"}
        return(paste0("A t2, ", variable, " will change by ",
                      change[2]," (", change[1], " - ", change[3], ")",
                      ", when compared to t1"))
      }
    } else {
      t.2 <- model.out$results[model.out$results$time == t2, variable]
      t.1 <- model.out$results[model.out$results$time == t1, variable]
      if (ratio) {
        change <- round((t.2 / t.1 * 100), 2)
        if (t.2 / t.1 > 1) {
          return(noquote(paste0('At t2, ', variable, ' will be ', change - 100,
                                '% higher than ', '(or ', change, '% times) ',
                                variable, ' at t1.')))
        } else {
          return(noquote(paste0('At t2, ', variable, ' will be equal to ', change,
                                '% of ', variable, ' at t1.')))
        }
      } else {
        change <- abs(round(t.2 - t.1, 2))
        net.change <- "decrease"
        if (t.2 - t.1 > 0) {net.change <- "increase"}
        return(paste0("A t2, ", variable, " will ", net.change, " by ", change,
                      ", when compared to t1"))
      }
    }
  }
}