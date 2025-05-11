create_confband <- function(x, func, confband_args){
  stopifnot(identical(func, "densfunc") || identical(func, "ecdf"),
            is.numeric(x), is.null(confband_args) || is.list(confband_args))
  density_args <- confband_args[["density_args"]]
  confband_args["density_args"] <- confband_args["curve_args"] <-
    confband_args["polygon_args"] <- confband_args["shade_col"] <- NULL

  if(identical(func, "densfunc")){
    densfunc <- function(x){
      dens <- do.call(density, args = combine_lists(list(x = x), density_args))
      output <- approxfun(x = dens$x, y = dens$y, yleft = 0, yright = 0)
      return(output)
    }
  }
  confband_default_args <- list(x = x, w = NULL, m = 100, batch_len = 50,
                                N = 5e4, conf.level = 0.95, buffer = 1e-4,
                                matsqrt_tol = default_distfreereg_tol()[["matsqrt_tol"]])
  confband_args <- combine_lists(list(func = get(func)), confband_args,
                                 confband_default_args)
  cb <- do.call(confband, args = confband_args)
  
  if(identical(func, "ecdf")) cb[["cb_upper"]] <- pmin(1, cb[["cb_upper"]])
  cb[["cb_lower"]] <- pmax(0, cb[["cb_lower"]])
  
  return(cb)
}
