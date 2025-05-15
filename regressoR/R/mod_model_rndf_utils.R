#' Error Evolution
#'
#' @param modelo a random forest model.
#' @param label a label plot.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_rf_error
#' @import echarts4r
#' @import traineR
#' @examples
#' model <- traineR::train.randomForest(Sepal.Length~., iris, mtry = 2, ntree = 20)
#' e_rf_error(model, "Trees")
#' 
#' 
e_rf_error <- function(modelo, label = "Trees") {
  data <- data.frame(x = c(1:length(modelo$mse)), RMSE = cbind(modelo$mse))
  data$RMSE <- sqrt(data$RMSE)
  
  plot.rf.err <- data |> 
    e_charts(x) |> 
    e_line(RMSE, lineStyle = list(type = 'dashed')) |> 
    e_legend(orient = 'vertical', right = '20', top = '10%') |>  
    e_axis_labels(x = label, y = 'Error') |>
    e_x_axis(scale = T) |> e_y_axis(scale = T) |>
    e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading() 
  
  plot.rf.err
}

#' Var importance Random Forest
#'
#' @param modelo a random forest model.
#' @param error a character specifying the type of importance.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_rndf_importance
#' @import echarts4r
#' @import traineR
#' @examples
#' model <- traineR::train.randomForest(Species~., iris, mtry = 2, ntree = 20)
#' e_rndf_importance(model)
#' 
e_rndf_importance <- function(modelo, error = "X.IncMSE") {
  aux <- data.frame(modelo$importance)
  aux[is.na(aux[[error]]), error] <- 0
  aux   <- aux[order(aux[[error]], decreasing = T), ]
  label <- row.names(aux)
  color <- gg_color_hue(length(label))
  aux   <- cbind(aux, label = label, color = color)
  res <- aux |> e_charts(label) |> e_bar_(error) |>  
    e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()|>
    e_add_nested("itemStyle", color) |> e_flip_coords() |> 
    e_y_axis(inverse = TRUE)
  return(res)
}
