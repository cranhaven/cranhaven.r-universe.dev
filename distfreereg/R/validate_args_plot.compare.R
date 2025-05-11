validate_args_plot.compare <-
function(object, which, stat, hlines, curve_args, density_args, confband_args,
         poly, legend, qqline){
  strict_match(which, choices = c("cdf", "dens","qq", "qqp"))
  strict_match(arg = stat, choices = object)
  if(!is.null(hlines) && !isFALSE(hlines) && !is.list(hlines))
    stop("\"hlines\" must be NULL, FALSE, or a list")
  if(!is.null(curve_args) && !is.list(curve_args))
    stop("\"curve_args\" must be NULL or a list")
  if(!is.null(density_args) && !is.list(density_args))
    stop("\"density_args\" must be NULL or a list")
  if(!is.null(confband_args) && !isFALSE(confband_args) && !is.list(confband_args))
    stop("confband_args must be NULL, FALSE, or a list")
  if(is.list(confband_args))
    validate_named_list(confband_args,
                        valid_names = c("w", "m", "batch_len", "N", "conf.level",
                                        "buffer", "lines_args", "polygon_args",
                                        "shade_col"))
  if(!is.null(poly) && !isFALSE(poly) && !is.list(poly))
    stop("\"poly\" must be NULL, FALSE, or a list")
  if(!is.null(legend) && !isFALSE(legend) && !is.list(legend))
    stop("\"legend\" must be NULL, FALSE, or a list")
  if(!is.null(qqline) && !isFALSE(qqline) && !is.list(qqline))
    stop("\"qqline\" must be NULL, FALSE, or a list")
}
