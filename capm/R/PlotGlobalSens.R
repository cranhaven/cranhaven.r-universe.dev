#' Plot results of GlobalSens function
#' @description Plot results of of \code{\link{CalculateGlobalSens}} function.
#' @param global.out output from \code{\link{CalculateGlobalSens}} function.
#' @param legend.label string with the name for the legend.
#' @param x.label string with the name for the x axis.
#' @param y.label string with the name for the y axis.
#' @param qt.label string with the name for the envelope calculated using the quantiles 0.05 and 0.95.
#' @param sd.label string with the name for the envelope calculated using the mean +- standard deviation ranges.
#' @param inner.color any valid specification of a color for the inner envelope.
#' @param outer.color any valid specification of a color for the outer envelope.
#' @details Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#'  
#' Other details of the plot can be modifyed using appropriate functions from \code{ggplot2} package.
#' @references Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @seealso \link[deSolve]{plot.deSolve}.
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
#' PlotGlobalSens(glob_all_solve_iasa)
#'
PlotGlobalSens <- function(global.out = NULL, x.label = 'Time', y.label = 'Population', legend.label = 'Sensitivity range', qt.label = 'Qt 0.05 - 0.95', sd.label = 'mean +- sd   ', inner.color = "DarkRed", outer.color = "LightBlue") {
  
  # Workaround to the "no visible binding for global variable" note.
  x <- Mean <- Min <- Max <- Sd <- q05 <- q95 <- NULL
  
  if (colnames(global.out)[length(global.out)] == 'param') {
    ggplot(global.out, aes(x = x, y = Mean)) +
      geom_ribbon(aes(ymin = q05, ymax = q95, fill = outer.color)) +
      geom_ribbon(aes(ymin = Mean - Sd, ymax = Mean + Sd, fill = inner.color)) +
      geom_line() + facet_wrap( ~ param) +
      xlab(x.label) + ylab(y.label) +
      scale_fill_manual(
        name = legend.label,
        values = c(inner.color, outer.color),
        labels = c(sd.label, qt.label)) +
      theme_minimal() +
      theme(legend.position = 'top')
  } else {
    ggplot(global.out, aes(x = x, y = Mean)) +
      geom_ribbon(aes(ymin = q05, ymax = q95, fill = outer.color)) +
      geom_ribbon(aes(ymin = Mean - Sd, ymax = Mean + Sd, fill = inner.color)) +
      geom_line() +
      xlab(x.label) + ylab(y.label) +
      scale_fill_manual(
        name = legend.label,
        values = c(inner.color, outer.color),
        labels = c(sd.label, qt.label)) +
      theme_minimal() +
      theme(legend.position = 'top')
  }
}
