validate_args_plot.distfreereg <-
function(object, stat, density_args, polygon_args, confband_args, abline_args, text_args){
  strict_match(arg = stat, choices = object)
  if(!is.null(density_args) && !is.list(density_args)) stop("density_args must be a list")
  if(!is.null(polygon_args) && !isFALSE(polygon_args) && !is.list(polygon_args))
    stop("polygon_args must be NULL, FALSE, or a list")
  if(!is.null(confband_args) && !isFALSE(confband_args) && !is.list(confband_args))
    stop("confband_args must be NULL, FALSE, or a list")
  if(is.list(confband_args)){
    validate_named_list(confband_args,
                        valid_names = c("w", "m", "batch_len", "N", "conf.level",
                                        "buffer", "lines_args", "polygon_args",
                                        "shade_col"))
    if(!is.null(confband_args[["w"]])) validate_numeric(x = confband_args[["w"]])
    if(!is.null(confband_args[["m"]])) validate_numeric(x = confband_args[["m"]], len = 1, pos_int = TRUE)
    if(!is.null(confband_args[["batch_len"]])) validate_numeric(x = confband_args[["batch_len"]], len = 1, pos_int = TRUE)
    if(!is.null(confband_args[["N"]])) validate_numeric(x = confband_args[["N"]], len = 1, pos_int = TRUE)
    if(!is.null(confband_args[["conf.level"]])) validate_numeric(x = confband_args[["conf.level"]], len = 1,
                                                                 min_val_strict = 0, max_val_strict = 1)
    if(!is.null(confband_args[["buffer"]])) validate_numeric(x = confband_args[["buffer"]], len = 1,
                                                                 min_val_strict = 0, max_val_strict = 0.5)
    if(!is.null(confband_args[["curve_args"]]) && !is.list(confband_args[["curve_args"]]))
      stop("confband_args[[\"curve_args\"]] must be NULL or a list")
    if(!is.null(confband_args[["polygon_args"]]) &&
       !isFALSE(confband_args[["polygon_args"]]) &&
       !is.list(confband_args[["polygon_args"]]))
      stop("confband_args[[\"polygon_args\"]] must be NULL, FALSE, or a list")
  }
  if(!is.null(abline_args) && !isFALSE(abline_args) && !is.list(abline_args))
    stop("abline_args must be NULL, FALSE, or a list")
  if(!is.null(text_args) && !isFALSE(text_args) && !is.list(text_args))
    stop("text_args must be NULL, FALSE, or a list")
}
