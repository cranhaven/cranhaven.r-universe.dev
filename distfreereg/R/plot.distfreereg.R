plot.distfreereg <-
  function(x, which = "dens", stat = NULL, density_args = NULL, polygon_args = NULL,
           confband_args = NULL, abline_args = NULL, shade_col = rgb(1,0,0,0.5),
           text_args = NULL, ...){
    strict_match(arg = which, choices = c("dens", "residuals", "epsp"))
    if(identical(which, "dens")){
      stopifnot(!is.null(names(x[["observed_stats"]])))
      if(is.null(stat)) stat <- names(x[["observed_stats"]])[1]
      validate_args_plot.distfreereg(object = x, stat = stat, density_args = density_args,
                                     polygon_args = polygon_args, confband_args = confband_args,
                                     abline_args = abline_args, text_args = text_args)
      stopifnot(!is.null(x[["mcsim_stats"]][[stat]]), !is.null(x[["observed_stats"]][[stat]]))
      default_density_args <- list(x = x[["mcsim_stats"]][[stat]], n = 2^11)
      density_args <- combine_lists(density_args, default_density_args)
      curve_data <- do.call(density, args = density_args)
      # If plotting confidence band, then create band here to get range for vertical
      # axis.
      if(!isFALSE(confband_args)){
        cb <- create_confband(x = default_density_args[["x"]], func = "densfunc",
                              confband_args = combine_lists(confband_args,
                                                            list(density_args = density_args)))
        y_max <- max(curve_data[["y"]], cb[["cb_upper"]])
      }
      plot_args <- combine_lists(list(...),
                                 list(x = curve_data,
                                      main = paste("Estimated", stat, "Statistic Density"),
                                      xlab = "Simulated Statistic Value"))
      if(!isFALSE(confband_args)) plot_args[["ylim"]] <- c(0, y_max)
      
      do.call(plot, plot_args)
      if(!isFALSE(confband_args)){
        plot_confband(cb = cb, confband_args = combine_lists(confband_args,
                                                             list(density_args = density_args)))
      }
      x_vals <- curve_data[["x"]]
      y_vals <- curve_data[["y"]]
      s <- x[["observed_stats"]][[stat]]
      if(!isFALSE(polygon_args)){
        default_polygon_args <- list(x = c(x_vals[x_vals >= s], max(x_vals), s),
                                     y = c(y_vals[x_vals >= s], 0, 0),
                                     col = shade_col, border = NA)
        polygon_args <- combine_lists(polygon_args, default_polygon_args)
        do.call(polygon, args = polygon_args)
      }
      if(!isFALSE(abline_args)){
        default_line_args <- list(v = s)
        line_args <- combine_lists(abline_args, default_line_args)
        do.call(abline, args = line_args)
      }
      if(!isFALSE(text_args)){
        default_text_args <- list(x = s, y = max(y_vals)/2, adj = c(1,0.5),
                                  labels = paste(" p =", x[["p"]][["value"]][[stat]], " "))
        text_args <- combine_lists(text_args, default_text_args)
        do.call(text, args = text_args)
      }
      output <- list(x = x_vals, y = y_vals)
      if(!isFALSE(confband_args)){
        output[["confband"]] <- cb
      }
      invisible(output)
    } else {
      n <- length(x[["data"]][["Y"]])
      default_args <- list(type = "l", col = "blue", ylab = "Value",
                           xlab = "Observation (Ordered)")
      if(identical(which, "residuals")){
        default_args[["x"]] <- seq_len(n)
        default_args[["y"]] <- residuals(x, type = "transformed")[x[["res_order"]]]
        default_args[["main"]] <- "Residual Values by Ordering"
        plot_args <- combine_lists(list(...), default_args)
        do.call(plot, plot_args)
      } else {
        if(identical(which, "epsp")){
          default_args[["x"]] <- seq_along(x[["epsp"]])
          default_args[["y"]] <- x[["epsp"]]
          default_args[["main"]] <- "Empirical Partial Sum Process"
          plot_args <- combine_lists(list(...), default_args)
          do.call(plot, plot_args)
        }
      }
    }
  }