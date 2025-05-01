#' Parameter ranges for global sensitivity analysis
#' @description Define the minimum and maximum values for parameters whose global sensitivities are to be assessed with \code{\link{CalculateGlobalSens}} or \code{\link{sensRange}} functions.
#' @param pars the same \code{pars} vector used in one of the following functions: \code{\link{SolveSI}} or \code{\link{SolveIASA}}.
#' @param range scale factor to define the minimum and maximum for each parameter. The default is 0.1, which set the minimum and maximum as 10 percent lesser and greater than the \code{pars} values.
#' @return \code{\link{data.frame}} with the complete set of parameter ranges.
#' @references Soetaert K and Petzoldt T (2010). Inverse modelling, sensitivity and monte carlo analysis in R using package FME. Journal of Statistical Software, 33(3), pp. 1-28.
#' 
#' Reichert P and Kfinsch HR (2001). Practical identifiability analysis of large environmental simulation models. Water Resources Research, 37(4), pp. 1015-1030.
#' 
#' Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @seealso \code{\link{sensRange}} and \code{\link{SolveSI}}.
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
#' ## Set ranges 10 % greater and lesser than the
#' ## point estimates.
#' rg_solve_iasa <- SetRanges(pars = dogs_iasa$pars)
#' 
SetRanges <- function(pars = NULL, range = 0.1) {
  par.ranges <- data.frame(min = c(pars) * (1 - range),
                           max = c(pars) * (1 + range))
  rownames(par.ranges) = names(pars)
  return(par.ranges)
}
