#' Global sensitivity analysis
#' @description Wraper for \code{\link{sensRange}} function, which calculates sensitivities of population sizes to parameters used in one of the following functions: \code{\link{SolveIASA}}, \code{\link{SolveSI}} or \code{\link{SolveTC}}.
#' @param model.out output from one of the previous function or a \code{\link{list}} with equivalent structure.
#' @param ranges output from the \code{\link{SetRanges}} function applied to the \code{pars} argument used in the function specified in \code{model.out}.
#' @param sensv string with the name of the output variables for which the sensitivity are to be estimated.
#' @param all logical. If \code{\link{FALSE}}, sensitivity ranges are calculated for each parameter. If \code{TRUE}, sensitivity ranges are calculated for the combination of all aparameters.
#' @details When \code{all} is equal to TRUE, \code{dist} argument in \code{\link{sensRange}} is defined as "latin" and when equal to \code{\link{FALSE}}, as "grid". The \code{num} argument in \code{\link{sensRange}} is defined as 100.
#' @return A \code{data.frame} (extended by \code{summary.sensRange} when \code{all == TRUE}) containing the parameter set and the corresponding values of the sensitivity output variables.
#' @references Soetaert K and Petzoldt T (2010). Inverse modelling, sensitivity and monte carlo analysis in R using package FME. Journal of Statistical Software, 33(3), pp. 1-28.
#' 
#' Reichert P and Kfinsch HR (2001). Practical identifiability analysis of large environmental simulation models. Water Resources Research, 37(4), pp.1015-1030.
#' 
#' Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @seealso \code{\link{sensRange}}.
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
#' ## Set ranges 10 % greater and lesser than the
#' ## point estimates.
#' rg_solve_iasa <- SetRanges(pars = dogs_iasa$pars)
#' 
#' ## Calculate golobal sensitivity of combined parameters.
#' ## To calculate global sensitivity to each parameter, set
#' ## all as FALSE.
#' glob_all_solve_iasa <- CalculateGlobalSens(
#'   model.out = solve_iasa_pt,
#'   ranges = rg_solve_iasa, 
#'   sensv = "n2", all = TRUE)
#' 
CalculateGlobalSens <- function(model.out = NULL, ranges = NULL, sensv = NULL, all = FALSE) {
  if (!setequal(rownames(ranges), names(model.out$pars))) {
    stop('All parameters in ranges must be\ncontained in "pars" argument  of model.out.')
  }
  if (length(intersect(sensv, names(model.out$init))) == 0) {
    stop('All variables in sensv must be\ncontained in "init" argument  of model.out.')
  }
  if (all) {
    sens <- sensRange(func = model.out$model, 
                      parms = model.out$pars, 
                      init = model.out$init,
                      time = model.out$time,
                      parRange = ranges,
                      dist = "latin", 
                      sensvar = sensv, num = 100)
    return(summary(sens))
  } else {
    sens <- NULL
    for (i in 1:length(model.out$pars)) {
      tmp <- sensRange(func = model.out$model, 
                       parms = model.out$pars, 
                       init = model.out$init,
                       time = model.out$time,
                       parRange = ranges[i, ],
                       dist = "grid", 
                       sensvar = sensv, num = 100)
      sens <- rbind(sens, summary(tmp))
    }
    param <- rep(names(model.out$pars), 
                 each = length(tmp[-1]))
    sens <- cbind(sens, param)
    return(sens)
  }
}