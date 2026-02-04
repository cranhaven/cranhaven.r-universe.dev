#' Error Evolution
#'
#' @param modelo a adabag model.
#' @param datos a data.frame object.
#' @param label a label plot.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_ada_evol_error
#' @import echarts4r
#' @import traineR
#' @examples
#' model <- traineR::train.adabag(Species~., iris, mfinal = 20, coeflearn = 'Freund')
#' e_ada_evol_error(model, iris)
#' 
e_ada_evol_error <- function(modelo, datos, label = "Iterations") {
  x <- error(modelo, datos)
  
  if (!((class(x) %in% c("errorevol"))))
    stop("x class should be errorevol")
  train <- x$error
  evolplot <- data.frame(x = c(1:length(x$error)), train = train)
  res <- evolplot |> e_charts(x) |> e_line(train) |> 
    e_title("Ensemble error vs number or trees", left = 'center',
            top = 5, textStyle = list(fontSize = 15)) |> 
    e_legend(orient = 'vertical', right = '20', top = '10%') |> 
    e_axis_labels(x = label, y = "Error") |>
    e_tooltip() |> e_datazoom(show = F) |> e_show_loading()
  return(res)
}

#' Var importance Random Forest
#'
#' @param modelo a adabag model.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_boost_importance
#' @import echarts4r
#' @import traineR
#' @examples
#' model <- traineR::train.adabag(Species~., iris, mfinal = 20, coeflearn = 'Freund')
#' e_boost_importance(model)
#' 
e_boost_importance <- function(modelo) {
  imp    <- modelo$importance 
  color  <- gg_color_hue(length(imp))
  aux    <- data.frame(importancia = imp, color = color)
  
  aux$nombre      <- row.names(aux) 
  aux$importancia <- abs(aux$importancia) 
  aux <- aux[order(aux$importancia, decreasing = T), ] # Ordena el df
  res <- aux |> e_charts(nombre) |> e_bar(importancia, name = var) |>   
    e_tooltip() |> e_datazoom(show = F) |> e_show_loading()|>
    e_add_nested("itemStyle", color) |> e_flip_coords() |>  
    e_y_axis(inverse = TRUE)
  return(res)
}

