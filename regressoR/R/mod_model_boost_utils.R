#' Error Evolution
#'
#' @param modelo a adabag model.
#' @param label a label plot.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_boost_evol_error
#' @import echarts4r
#' @import traineR
#' @examples
#' model <- traineR::train.gbm(Sepal.Length~., data = iris, 
#'   distribution = "gaussian", n.trees = 5, shrinkage = 0.01)
#' e_boost_evol_error(model, iris)
#' 
e_boost_evol_error <- function(modelo, label = "Iterations") {
  res <- data.frame(x = 1:modelo$n.trees, y = modelo$train.error)
  
  res |> e_charts(x) |> e_line(y) |> 
    e_title("Ensemble error vs number or trees", left = 'center',
            top = 5, textStyle = list(fontSize = 15)) |> 
    e_legend(orient = 'vertical', right = '20', top = '10%') |> 
    e_axis_labels(x = label, y = "RMSE") |>
    e_tooltip() |> e_datazoom(show = F) |> e_show_loading()
}

#' Var importance Adabag
#'
#' @param modelo a adabag model.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_boost_importance
#' @import echarts4r
#' @import traineR
#' @examples
#' model <- traineR::train.gbm(Sepal.Length~., data = iris, 
#'   distribution = "gaussian", n.trees = 5, shrinkage = 0.01)
#' e_boost_importance(model)
#' 
e_boost_importance <- function(modelo) {
  imp   <- gbm::summary.gbm(modelo, order = T, plotit = F)
  color <- gg_color_hue(nrow(imp))
  aux   <- data.frame(importancia = imp, color = color)
  
  aux$nombre      <- row.names(aux) 
  aux$importancia <- abs(aux$importancia.rel.inf)
  aux <- aux[order(aux$importancia, decreasing = T), ] # Ordena el df
  res <- aux |> e_charts(nombre) |> e_bar(importancia, name = var) |>   
    e_tooltip() |> e_datazoom(show = F) |> e_show_loading()|>
    e_add_nested("itemStyle", color) |> e_flip_coords() |>  
    e_y_axis(inverse = TRUE)
  return(res)
}

