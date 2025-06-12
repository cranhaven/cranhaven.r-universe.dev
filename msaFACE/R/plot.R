plot.MSA_coef <- function(x, i_var = 1:length(x), main_plot = TRUE, i_acc = 1, axis.param = list(), label.param = list(), ...)
{ 
  if (main_plot)
  {
    if (length(i_var) > 1)
    {
      if (all(par()$mfrow[1] == 1, par()$mfrow[2] == 1))
        par(mfrow = rep.int(ceiling(sqrt(length(i_var))), 2))
      for (i in i_var)
        plot(x, i_var = i, main_plot = main_plot, axis.param = axis.param, 
             label.param = label.param, force_main = names(x)[i], 
             plot_legend = i == i_var[1], ...)
    } else {      
      if (!is.numeric(i_var))
        i_var <- which(names(x) == i_var)
      ## Search for changes in defaults passed in "..."-parameters
      dots <- .get_ellipsis(x, list(...), i_var, axis.param, main_plot, i_acc)
    
#   x_rearr <- attr(x, "allData")$data
                
      ## Define Point size
      point_size <- rep.int(par()$cex, length(x[[i_var]][,2]))
      point_size[x[[i_var]][, 2] <= 0.001                         ] <- 2.5 * par()$cex
      point_size[x[[i_var]][, 2] < 0.01 & x[[i_var]][ ,2] >= 0.001] <- 1.5 * par()$cex
      point_size[x[[i_var]][, 2] >= 0.05                          ] <- 0.5 * par()$cex

      ####################### 
      ### Plot main plot
#       par(mar = c(4,4,2,4))
      plot(x = x[[i_var]][,i_var + 3],
           y = x[[i_var]]$CFE,
           xlim = dots$xlim,
           ylim = dots$ylim1,
           cex = point_size,
           axes = dots$axis,
           ylab = "",
           xlab = "",
           type = "p",
           pch = dots$pch,
           col = dots$col_main,
           main = dots$main)

      ## Plot boundaries for environmental regimes defined by mean and mean +/- 1 sd of subsetwise i_var
      abline(v = mean(x[[i_var]][,i_var + 3]) - sd(x[[i_var]][,i_var + 3]),  lty = "dashed")
      abline(v = mean(x[[i_var]][,i_var + 3]),  lty = "dashed")
      abline(v = mean(x[[i_var]][,i_var + 3]) + sd(x[[i_var]][,i_var + 3]),  lty = "dashed")
      
      ## Plot horizontal lines for the range of i_var
      for (n in 1:nrow(x[[i_var]])) 
      {
        arrows(x0 = x[[i_var]][,ncol(x[[i_var]])-1],
               x1 = x[[i_var]][,ncol(x[[i_var]])],
               y0 = x[[i_var]]$CFE,
               col = dots$col_main, length = .05, angle = 90, code = 3, lwd = dots$lwd[1])
      }

      ## Plot axes
      if (length(axis.param) > 0)
      {
        axis.param[[1]]$side <- 1
        do.call("axis",  axis.param[[1]])
      }
      if (length(axis.param) > 1)
      {
        axis.param[[2]]$side <- 2
        do.call("axis",  axis.param[[2]])
      }
    

      ## Plot labels
      if (length(label.param) == 0)
      {
        mtext(dots$xlab,  side = 1, line = par()$mgp[1], col = dots$col_axis[1])
        mtext(dots$ylab1, side = 2, line = par()$mgp[1], col = dots$col_axis[2])
        mtext(dots$ylab2, side = 4, line = par()$mgp[1], col = dots$col_axis[3])
      } else {
        if (length(label.param) > 0)
        {
          label.param[[1]]$text <- dots$xlab
          do.call("mtext",  label.param[[1]])
        }
        if (length(label.param) > 1)
        {
          label.param[[2]]$text <- dots$ylab1
          do.call("mtext",  label.param[[2]])
        }
        if (length(label.param) > 2)
        {
          label.param[[3]]$text <- dots$ylab2
          do.call("mtext",  label.param[[3]])
        }
      }
      
      old.pars <- par()
      ## Plot average response variable
      par(new = T)
      plot(x[[i_var]][,3] ~ x[[i_var]][,i_var + 3],
           xlim = dots$xlim,
           ylim = dots$ylim2,
           axes = FALSE,
           ylab = "",
           xlab = "",
           type = "l",
           lwd = dots$lwd[2],
           col = dots$col_axis[3])

      ## Add second y-axis
      if (length(axis.param) > 2)
      {
        axis.param[[3]]$side <- 4
        do.call("axis",  axis.param[[3]])
      } else {
        if (dots$axis)
          axis(4, col.axis = dots$col_axis[3], col = dots$col_axis[3])
      }
      if (dots$plotlegend)
      {
        if (is.null(dots$legend$x))
          legend_vals <- .get_legend_position(x, i_var, list(usr1 = old.pars$usr, usr2 = par()$usr), dots$legend)
        do.call("legend", legend_vals)
      }
    }
  } else {
    if (length(i_var) > 1)
      stop("Plotting of accompanying variables for multiple i_var not useful")
    if (!is.numeric(i_var))
      i_var <- which(names(x) == i_var)
    ## Plot accompanying variables
    if (length(i_acc) > 1)
    {
      if (all(par()$mfrow[1] == 1, par()$mfrow[2] == 1))
        par(mfrow = rep.int(ceiling(sqrt(length(i_acc))), 2))
      for (i in i_acc)
        plot(x, i_var = i_var, i_acc = i, main_plot = main_plot, axis.param = axis.param,
             label.param = label.param, force_main = names(x)[i], ...)
    } else {
      if (!is.numeric(i_acc))
        i_acc <- which(names(x) == i_acc)
      if (i_var == i_acc)
      {
        if (all(par()$mfrow[1] == 1, par()$mfrow[2] == 1))
        {
          stop("Plotting the same variable on x- and y-axis not useful")
        } else {
          plot(1, 1, type = "n", axes = FALSE, ylab = "", xlab = "")
        }
      } else {
        ## Search for changes in defaults passed in "..."-parameters
        dots <- .get_ellipsis(x, list(...), i_var, axis.param, main_plot, i_acc)

        ## Draw plot
        plot(y = x[[i_var]][,i_acc + 3],
             x = x[[i_var]][,i_var + 3],
             xlim = dots$xlim,
             axes = dots$axis,
             ylab = "",
             xlab = "",
             pch = dots$pch,
             cex = dots$cex)
        
        ## Plot axes
        if (length(axis.param) > 0)
        {
          axis.param[[1]]$side <- 1
          do.call("axis",  axis.param[[1]])
        }
        if (length(axis.param) > 1)
        {
          axis.param[[2]]$side <- 2
          do.call("axis",  axis.param[[2]])
        }
        
        ## Plot labels
        if (length(label.param) == 0)
        {
          mtext(dots$xlab,  side = 1, line = par()$mgp[1], col = dots$col_axis[1])
          mtext(dots$ylab1, side = 2, line = par()$mgp[1], col = dots$col_axis[2])
        } else {
          if (length(label.param) > 0)
          {
            label.param[[1]]$text <- dots$xlab
            do.call("mtext",  label.param[[1]])
          }
          if (length(label.param) > 1)
          {
            label.param[[2]]$text <- dots$ylab1
            do.call("mtext",  label.param[[2]])
          }
        }
      }
    }
  }
}

