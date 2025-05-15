#' e.rdim.rmse
#' 
#' @description graph the root mean square error of cross validation according to components used.
#'
#' @param modelo a dimension reduction model.
#' @param ncomp the optimum number of components.
#' @param titles labels on the chart
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' 
#' @export
#' 
e.rdim.rmse <- function(modelo, ncomp, titles = c("RMSE", "Componente")) {
  
  errores <- RMSEP(modelo)$val[1, 1, -1]
  df <- data.frame(ncomp = 1:length(errores), rmse = errores)
  
  tooltip <- e_JS(paste0(
    "function(params) {",
    "  var componente = '<b>", titles[2], ": </b>' + params.value[0];",
    "  var rmse = '<b>", titles[1], ": </b>' + params.value[1].toFixed(4);",
    "  return(componente + '<br/>' + rmse)",
    "}"
  ))
  
  res <- df |> e_charts(ncomp) |>
    e_line(rmse) |>
    e_mark_line(data = list(xAxis = ncomp), title = ncomp) |>
    e_x_axis(name = titles[2]) |>
    e_y_axis(name = titles[1]) |>
    e_legend(show = FALSE) |>
    e_tooltip(formatter = tooltip) |>
    e_datazoom(show = FALSE) |>
    e_show_loading()
  
  return(res)
}

#' plot_pred_rd
#' 
#' @description graph of variance explained in the predictors according to components used.
#'
#' @param modelo a dimension reduction model.
#' @param ncomp the optimum number of components.
#' @param titles labels on the chart
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' @export
#' 
e.rdim.vare <- function(modelo, ncomp, titles = c("Varianza Explicada", "Componente")) {
  var.explicada <- cumsum(explvar(modelo)) / 100
  df <- data.frame(ncomp = 1:length(var.explicada), vare = var.explicada * 100)
  
  tooltip <- e_JS(paste0(
    "function(params) {",
    "  var vare = '<b>", titles[2], ": </b>' + params.value[0];",
    "  var rmse = '<b>", titles[1], ": </b>' + params.value[1].toFixed(4);",
    "  return(vare + '<br/>' + rmse)",
    "}"
  ))
  
  res <- df |> e_charts(ncomp) |>
    e_line(vare) |>
    e_mark_line(data = list(xAxis = ncomp), title = ncomp) |>
    e_x_axis(name = titles[2]) |>
    e_y_axis(name = titles[1]) |>
    e_legend(show = FALSE) |>
    e_tooltip(formatter = tooltip) |>
    e_datazoom(show = FALSE) |>
    e_show_loading()
  
  return(res)
}