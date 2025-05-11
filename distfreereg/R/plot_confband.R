plot_confband <- function(cb, confband_args){
  stopifnot(is.null(confband_args) || is.list(confband_args))
  density_args <- confband_args[["density_args"]]
  curve_args <- confband_args[["curve_args"]]
  polygon_args <- confband_args[["polygon_args"]]
  stopifnot(is.null(density_args) || is.list(density_args),
            is.null(curve_args) || is.list(curve_args),
            is.null(polygon_args) || is.list(polygon_args))
  shade_col <- if(is.null(confband_args[["shade_col"]])) rgb(0,0,0,0.15) else confband_args[["shade_col"]]

  lower_curve_args_default <- list(lty = "dotted")
  upper_curve_args_default <- list(lty = "dotted")
  lower_curve_args <- combine_lists(curve_args[["lower"]], lower_curve_args_default)
  upper_curve_args <- combine_lists(curve_args[["upper"]], upper_curve_args_default)
  curve_args[["lower"]] <- curve_args[["upper"]] <- NULL
  lower_curve_args <- combine_lists(list(x = cb[["w"]], y = cb[["cb_lower"]]),
                                curve_args, lower_curve_args)
  upper_curve_args <- combine_lists(list(x = cb[["w"]], y = cb[["cb_upper"]]),
                                curve_args, upper_curve_args)

  do.call(lines, args = lower_curve_args)
  do.call(lines, args = upper_curve_args)

  if(!isFALSE(polygon_args)){
    default_polygon_args <- list(x = c(cb[["w"]], rev(cb[["w"]])),
                                 y = c(cb[["cb_upper"]], rev(cb[["cb_lower"]])),
                                 col = shade_col, border = NA)
    polygon_args <- combine_lists(polygon_args, default_polygon_args)
    do.call(polygon, args = polygon_args)
  }
  
  invisible(cb)
}
