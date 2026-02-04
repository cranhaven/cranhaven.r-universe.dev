#' Var importance XGBoosting
#'
#' @param modelo a random forest model.
#' @param error a character specifying the type of importance.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_xgb_importance
#' @import echarts4r
#' @import traineR
#' 
e_xgb_importance <- function(modelo, error = "Gain") {
  
  ## ==============================
  ## SOPORTE WRAPPER XGBOOST
  ## ==============================
  booster <- if (inherits(modelo, "xgb.Booster.prmdt")) {
    modelo$model
  } else {
    modelo
  }
  
  x   <- booster$feature_names
  aux <- xgboost::xgb.importance(
    feature_names = x,
    model = booster
  )
  
  aux <- data.frame(aux)
  aux <- na.omit(aux)
  
  if (is.null(aux[[error]])) {
    error <- "Weight"
    aux <- aux |>
      dplyr::group_by(Feature) |>
      dplyr::summarise(
        Weight = sum(Weight),
        .groups = 'drop'
      ) |>
      data.frame()
  }
  
  aux[[error]] <- abs(aux[[error]])
  aux <- aux[order(aux[[error]], decreasing = TRUE), ]
  
  color <- gg_color_hue(nrow(aux))
  aux[["color"]] <- color
  
  res <- aux |>
    e_charts(Feature) |>
    e_bar_(error) |>
    e_tooltip() |>
    e_datazoom(show = FALSE) |>
    e_show_loading() |>
    e_add_nested("itemStyle", color) |>
    e_flip_coords() |>
    e_y_axis(inverse = TRUE)
  
  return(res)
}

