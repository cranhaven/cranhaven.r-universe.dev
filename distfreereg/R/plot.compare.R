plot.compare <-
  function(x, y, ..., which = "cdf", stat = NULL, hlines = NULL, curve_args = NULL,
           confband_args = FALSE, density_args = NULL, poly = NULL,
           legend = NULL, qqline = NULL){
    # NOTE!!! "x" and "y" values used below DO NOT refer to horizontal and
    # vertical coordinates, but rather to values specified by the "x" and "x"
    # arguments.
    if(is.null(stat)) stat <- names(x[["observed_stats"]])[1]
    validate_args_plot.compare(object = x, which = which, stat = stat,
                               hlines = hlines, curve_args = curve_args,
                               confband_args = confband_args,
                               density_args = density_args, poly = poly,
                               legend = legend, qqline = qqline)
    stopifnot(!is.null(x[["observed_stats"]][[stat]]))
    xvals <- x[["observed_stats"]][[stat]]
    xname <- deparse(substitute(x))
    x_length <- length(xvals)
    n <- length(x[["Y_mean"]])
    if(missing(y)){
      stopifnot(!is.null(x[["mcsim_stats"]][[stat]]))
      yvals <- x[["mcsim_stats"]][[stat]]
    } else {
      stopifnot(!is.null(y[["observed_stats"]][[stat]]))
      if(!inherits(y, "compare")) stop("y must have class 'compare'")
      strict_match(arg = stat, choices = y)
      if(which == "qqp") stop("Cannot create p-value Q-Q plot with two 'compare' objects")
      yname <- deparse(substitute(y))
      if(identical(xname, yname)) stop("The same object cannot be supplied to 'x' and 'y'")
      if(n != length(y[["Y_mean"]])) warning("Length of Y_mean different for ",
                                             xname, " and ", yname)
      yvals <- y[["observed_stats"]][[stat]]
      if(x_length != length(yvals)) warning("Length of observed stats different for ",
                                     xname, " and ", yname)
    }
    y_length <- length(yvals)
    if(isTRUE(which == "cdf")){
      xvals <- sort(xvals)
      yvals <- sort(yvals)
      main <- if(missing(y)){
        paste0("Observed and simulated ", stat, " CDFs (n=", n, ", reps=", x_length, ")")
      } else {
        paste0("Observed ", stat, " CDFs")
      }
      # Define default arguments for creating plot region.
      default_args <- list(x = NULL, type = "n", ylim = c(0,1),
                           xlim = c(min(xvals, yvals), max(xvals, yvals)),
                           xlab = expression(s), ylab = bquote(expression(P(.(stat)<=s))),
                           main = main)
      plot_args <- combine_lists(list(...), default_args)
      do.call(plot, args = plot_args)
      
      # Deal with horizontal lines at 0 and 1.
      if(!isFALSE(hlines)){
        default_abline_args <- list(h = 0:1, lty = 2, col = "gray")
        abline_args <- combine_lists(hlines, default_abline_args)
        do.call(abline, args = abline_args)
      }
      
      # Create argument lists for creating cdf curves.
      default_obs_lines_args <- list(x = xvals,
                                     y = (1:length(xvals))/length(xvals),
                                     lty = 1, col = "blue", type = "l", 
                                     xlim = c(min(xvals), max(xvals)))
      default_mcsim_lines_args <- list(x = yvals,
                                       y = (1:length(yvals))/length(yvals),
                                       lty = 2, col = "red", type = "l",
                                       xlim = c(min(yvals), max(yvals)))
      # First add arguments in [["obs"]] and [["mcsim"]] elements to their respective lists
      obs_lines_args <- combine_lists(curve_args[["obs"]], default_obs_lines_args)
      mcsim_lines_args <- combine_lists(curve_args[["mcsim"]], default_mcsim_lines_args)
      
      # Now remove [["obs"]] and [["mcsim"]], and add what remains to both lists.
      curve_args[["obs"]] <- curve_args[["mcsim"]] <- NULL
      obs_lines_args <- combine_lists(curve_args, obs_lines_args)
      mcsim_lines_args <- combine_lists(curve_args, mcsim_lines_args)
      
      # Before plotting cdf curves, plot confidence bands, if called for.
      if(!isFALSE(confband_args)){
        cbx <- create_confband(x = sample(xvals), func = "ecdf", confband_args = confband_args)
        cby <- create_confband(x = sample(yvals), func = "ecdf", confband_args = confband_args)
        plot_confband(cb = cbx, confband_args = confband_args)
        plot_confband(cb = cby, confband_args = confband_args)
      }
      
      do.call(lines, obs_lines_args)
      do.call(lines, mcsim_lines_args)
      
      if(!isFALSE(legend)){
        default_legend_args <- list(x = "bottomright", bg = "white",
                                    col = c(obs_lines_args[["col"]], mcsim_lines_args[["col"]]),
                                    lty = c(obs_lines_args[["lty"]], mcsim_lines_args[["lty"]]))
        default_legend_args[["legend"]] <- if(missing(y)){
          c("observed", "simulated")
        } else {
          c(xname, yname)
        }
        legend_args <- combine_lists(legend, default_legend_args)
        do.call(graphics::legend, args = legend_args)
      }
      output <- list(list(x = xvals, y = 1:x_length/x_length),
                     list(x = yvals, y = 1:y_length/y_length))
      if(missing(y)){
        names(output)[1:2] <- c("observed", "simulated")
      } else {
        names(output)[1:2] <- c(xname, yname)
      }
      if(!isFALSE(confband_args)){
        output[["confband_x1"]] <- cbx
        output[["confband_x2"]] <- cby
        if(missing(y)){
          names(output)[3:4] <- c("confband_observed", "confband_simulated")
        } else {
          names(output)[3:4] <- c(paste("confband", xname, sep = "_"),
                                  paste("confband", yname, sep = "_"))
        }
      }
      invisible(output)
    } else {
      if(isTRUE(which == "dens")){
        obs_density_args <- combine_lists(density_args[["obs"]], list(x = xvals))
        mcsim_density_args <- combine_lists(density_args[["mcsim"]], list(x = yvals))
        density_args[["obs"]] <- density_args[["mcsim"]] <- NULL
        obs_density_args <- combine_lists(density_args, obs_density_args)
        mcsim_density_args <- combine_lists(density_args, mcsim_density_args)
        dens_x <- do.call(density, args = obs_density_args)
        dens_y <- do.call(density, args = mcsim_density_args)
        main <- if(missing(y)){
          paste0("Observed and simulated ", stat, " Densities (n=", n, ", reps=", x_length, ")")
        } else {
          paste0("Observed ", stat, " Densities")
        }
        default_args <- list(x = NULL, type = "n",
                             xlim = c(min(dens_x[["x"]], dens_y[["x"]]), max(dens_x[["x"]], dens_y[["x"]])),
                             ylim = c(0, max(dens_x[["y"]], dens_y[["y"]])),
                             xlab = "Statistic", ylab = "Density",
                             main = main)
        plot_args <- combine_lists(list(...), default_args)
        do.call(plot, args = plot_args)
        
        if(!isFALSE(poly)){
          default_obs_polygon_args <- list(x = dens_x[["x"]], y = dens_x[["y"]],
                                           col = rgb(0,0,1,0.5))
          obs_polygon_args <- combine_lists(poly[["obs"]], default_obs_polygon_args)
          default_mcsim_polygon_args <- list(x = dens_y[["x"]], y = dens_y[["y"]],
                                             col = rgb(1,0,0,0.5))
          mcsim_polygon_args <- combine_lists(poly[["mcsim"]], default_mcsim_polygon_args)
          poly[["obs"]] <- poly[["mcsim"]] <- NULL
          obs_polygon_args <- combine_lists(poly, obs_polygon_args)
          mcsim_polygon_args <- combine_lists(poly, mcsim_polygon_args)
          do.call(polygon, args = obs_polygon_args)
          do.call(polygon, args = mcsim_polygon_args)
        }
        
        obs_lines_args <- combine_lists(curve_args[["obs"]],
                                        list(x = dens_x[["x"]], y = dens_x[["y"]]))
        mcsim_lines_args <- combine_lists(curve_args[["mcsim"]],
                                          list(x = dens_y[["x"]], y = dens_y[["y"]]))
        
        # Now remove [["obs"]] and [["mcsim"]], and add what remains to both lists.
        curve_args[["obs"]] <- curve_args[["mcsim"]] <- NULL
        obs_lines_args <- combine_lists(curve_args, obs_lines_args)
        mcsim_lines_args <- combine_lists(curve_args, mcsim_lines_args)
        
        # Before plotting density curves, plot confidence bands, if called for.
        if(!isFALSE(confband_args)){
          confband_args[["density_args"]] <- density_args
          cbx1 <- create_confband(x = sample(xvals), func = "densfunc",
                                  confband_args = combine_lists(confband_args,
                                                                list(density_args = obs_density_args)))
          cbx2 <- create_confband(x = sample(yvals), func = "densfunc",
                                  confband_args = combine_lists(confband_args,
                                                                list(density_args = mcsim_density_args)))
          plot_confband(cb = cbx1, confband_args = combine_lists(confband_args,
                                                                 list(density_args = mcsim_density_args)))
          plot_confband(cb = cbx2, confband_args = combine_lists(confband_args,
                                                                 list(density_args = mcsim_density_args)))
        }
        
        do.call(lines, obs_lines_args)
        do.call(lines, mcsim_lines_args)
        
        if(!isFALSE(legend)){
          default_legend_args <- list(x = "bottomright", bg = "white",
                                      fill = c(obs_polygon_args[["col"]], mcsim_polygon_args[["col"]]))
          default_legend_args[["legend"]] <- if(missing(y)){
            c("observed", "simulated")
          } else {
            c(xname, yname)
          }
          legend_args <- combine_lists(legend, default_legend_args)
          do.call(graphics::legend, args = legend_args)
        }
        output <- list(list(x = dens_x[["x"]], y = dens_x[["y"]]),
                       list(x = dens_y[["x"]], y = dens_y[["y"]]))
        if(missing(y)){
          names(output)[1:2] <- c("observed", "simulated")
        } else {
          names(output)[1:2] <- c(xname, yname)
        }
        if(!isFALSE(confband_args)){
          if(missing(y)){
            output[["confband_observed"]] <- cbx1
            output[["confband_simulated"]] <- cbx2
          } else {
            output[[paste("confband", xname, sep = "_")]] <- cbx1
            output[[paste("confband", yname, sep = "_")]] <- cbx2
          }
        }
        invisible(output)
      } else {
        if(isTRUE(which == "qq")){
          if(missing(y)){
            xlab <- "Observed Statistics"
            ylab <- "Simulated Statistics"
          } else {
            xlab <- paste("Observed", xname)
            ylab <- paste("Observed", yname)
          }
          default_args <- list(x = xvals, y = yvals, xlab = xlab,
                               ylab = ylab, main = paste0("Q-Q Plot for ", stat,
                                                          " (n=", n, ", reps=", x_length, ")"))
          qqplot_args <- combine_lists(list(...), default_args)
          output <- do.call(qqplot, args = qqplot_args)
          if(!isFALSE(qqline)){
            default_abline_args <- list(a = 0, b = 1, col = "red")
            abline_args <- combine_lists(qqline, default_abline_args)
            do.call(abline, args = abline_args)
          }
          invisible(output)
        } else {
          if(isTRUE(which == "qqp")){
            default_args <- list(x = punif(ppoints(length(x[["p"]][[stat]]))),
                                 y = x[["p"]][[stat]],
                                 xlab = "Uniform Quantiles",
                                 ylab = "P-Values",
                                 main = paste0("P-Values and Uniform Quantiles for ",
                                               stat, " (n=", n, ", reps=", x_length, ")"))
            qqplot_args <- combine_lists(list(...), default_args)
            output <- do.call(qqplot, args = qqplot_args)
            if(!isFALSE(qqline)){
              default_abline_args <- list(a = 0, b = 1, col = "red")
              abline_args <- combine_lists(qqline, default_abline_args)
              do.call(abline, args = abline_args)
            }
            invisible(output)
          } else {
            stop("Invalid value of \"which\"")
          }
        }
      }
    }
  }
