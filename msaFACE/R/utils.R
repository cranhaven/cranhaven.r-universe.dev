print.MSA_coef <- function(x, ...)
{
  nam <- as.character(names(x))
  cat("\n")
  cat(paste("################################################",
            "Object of class 'MSA_coef' containing",
            paste0(length(nam), " environmental variables"),
            sep = "\n"))
  cat("\n\n")
  
  left_col_width <- max(nchar(nam)) + 4
  max_width_ivar <- nchar(as.character(length(nam)))
  if (left_col_width < 20)
    left_col_width <- 20
  max_width <- left_col_width + max(c(11, max_width_ivar))
  if (max_width > 40)
  {
    left_col_width <- 40 - max_width_ivar - 4
    max_width <- 40
  }  
  
  cat("The following environmental variables are\navailable:\n")
  cat(paste(rep.int("-", max_width), collapse = "", sep = ""), "\n")
  cat(paste("Variable", 
            paste(rep.int(" ", left_col_width - nchar("Variable")), collapse = ""),
            "i_var/i_acc\n", sep = ""))
  cat(paste(rep.int("-", max_width), collapse = "", sep = ""), "\n")
  for (i in 1:length(nam))
    cat(paste(nam[i], 
              paste(rep.int(" ", 
                            left_col_width - nchar(nam[i]) + 11 - nchar(as.character(i))),
                    collapse = ""),
              i, "\n", collapse = "", sep = ""))
  cat(paste(rep.int("-", max_width), collapse = "", sep = ""), "\n")
  cat("\nFor detailed information of the output coefficients\ntype 'summary()'\n")
}

show.MSA_coef <- function(object, ...)
  print(object, ...)

summary.MSA_coef <- function(object, ...)
{
  x <- object
  coefficients <- data.frame(var_name = names(x),
                             CFE_max = as.character(1:length(x)),
                             CFE_min = as.character(1:length(x)),
                             envCon_of_CFE_max =  as.character(1:length(x)),
                             envCon_of_CFE_min =  as.character(1:length(x)),
                             depVar_max =  as.character(1:length(x)),
                             depVar_min =  as.character(1:length(x)),
                             stringsAsFactors = FALSE)
  for (i in 1:length(x)){
  coefficients$CFE_max[i] <- prettyNum(max(x[[i]][,1]), digits = 2)
  coefficients$CFE_min[i] <- prettyNum(min(x[[i]][,1]), digits = 2)
  coefficients$envCon_of_CFE_max[i] <- prettyNum(x[[i]][which.max(x[[i]][,1]), i+3], digits = 3)
  coefficients$envCon_of_CFE_min[i] <- prettyNum(x[[i]][which.min(x[[i]][,1]), i+3], digits = 3)
  coefficients$depVar_max[i] <- prettyNum(max(x[[i]][,3]), digits = 3)
  coefficients$depVar_min[i] <-  prettyNum(min(x[[i]][,3]), digits = 3)
  }
  
  cat(paste("####################################################################################################",
            "Regression coefficients of the moving subset analysis on the influence of ",
            paste0("different (forcing) experiment-support variables (n = ", length(x), ")\n\n"), sep = "\n"))
  
  .formatTable(c("Variable","CFEmax","CFEmin","Cond_for_CFEmax","Cond_for_CFEmin",
                 "Dependent_Max","Dependent_Min"), coefficients)
}
.get_subsets <- function(n, window_size, increment)
  apply(matrix(0:((n-window_size)/increment), ncol = 1), 1, function(i, window_size, increment)
  {
    c(1:window_size) + (i * increment)
  }, window_size, increment) 
  
  
.get_ellipsis <- function(x, dots, i_var, axis.param, main_plot, i_acc)
{
  res <- list()
  ## 1. Labels of axis
  if (any(names(dots) == "xlab"))
  {
    res$xlab <- dots$xlab
  } else {
    if (main_plot)
    {
      res$xlab <- names(x)[i_var]
    } else {
      res$xlab <- colnames(x[[i_var]][i_var + 3])
    }
  }
  
  if (any(names(dots) == "ylab"))
  {
    res$ylab1 <- dots$ylab[1]
    if (length(dots$ylab) > 1)
    {
      res$ylab2 <- dots$ylab[2]
    } else {
      res$ylab2 <- attr(x, "allData")$response_var
    }
  } else {
    if (main_plot)
    {
      res$ylab1 <- "CFE"
    } else {
      res$ylab1 <- colnames(x[[i_var]][i_acc + 3])
    }
    res$ylab2 <- attr(x, "allData")$response_var
  }
  
  ## Main title
  if (any(names(dots) == "main"))
  {
    res$main <- dots$main
  } else {
    if (!any(names(dots) == "force_main"))
    {
      res$main <- ""
    } else {
      res$main <- dots$force_main
    }
  }
  
  ## Axis limits
  if (any(names(dots) == "xlim"))
  {
    res$xlim <- dots$xlim
    res$got_xlim <- TRUE
  } else {
    res$xlim <- c(min(x[[i_var]][,ncol(x[[i_var]])-1]), max(x[[i_var]][,ncol(x[[i_var]])]))
    res$got_xlim <- FALSE
  }
  
  if (any(names(dots) == "ylim"))
  {
    if (is.list(dots$ylim))
    {
      res$ylim1 <- dots$ylim[[1]]
      res$ylim2 <- dots$ylim[[2]]
    } else {
      res$ylim1 <- dots$ylim
      res$ylim2 <- range(x[[i_var]][,3], na.rm = TRUE)
    }
  } else {
    res$ylim1 <- range(x[[i_var]]$CFE, na.rm = TRUE)
    res$ylim2 <- range(x[[i_var]][,3], na.rm = TRUE)
  }

  ## Default axis to be plotted?
  if (any(names(dots) == "axis"))
  {
    res$axis <- dots$axis
  } else {
    res$axis <- TRUE
  }
  if (length(axis.param) > 0)
    res$axis <- FALSE

  ## Point and boundaries colors
  if (any(names(dots) == "col"))
  {
    res$col_main <- dots$col
  } else {
    res$col_main <- "green4"
  }
  
  ## Point symbol
  if (any(names(dots) == "pch"))
  {
    res$pch <- dots$pch
  } else {
    res$pch <- 19
  }
  
  ## Line width (1st element = line of range bars, 2nd element line of response_var)
  if (any(names(dots) == "lwd"))
  {
    if (length(dots$lwd) > 1)
    {
      res$lwd <- dots$lwd[1:2]
    } else {
      res$lwd <- rep.int(dots$lwd[1], 2)
    }
  } else {
    res$lwd <- c(1,2)
  }
  
  ## Axes colors
  if (any(names(dots) == "col.axis"))
  {
    res$col_axis <- rep.int(dots$col.axis[1], 3)
    if (length(dots$col.axis) > 1)
      res$col_axis[2] <- dots$col.axis[2]
    if (length(dots$col.axis) > 2)
      res$col_axis[3] <- dots$col.axis[3]
  } else {
    res$col_axis <- c("black", "black", "gray50")
  } 
  
  ## Cex
  if (any(names(dots) == "cex"))
  {
    res$cex <- dots$cex
  } else {
    res$cex <- 1
  } 
  
  
  ## Legend
  if (any(names(dots) == "legend"))
  {
    if (is.logical(dots$legend))
    {
      if (dots$legend[1])
      {
        res$legend <- list(legend = c("p < 0.001", "p < 0.01", "p < 0.05", "p >= 0.05"),
                           pt.cex = c(2.5 * par()$cex, 1.5 * par()$cex, par()$cex, 0.5 * par()$cex),
                           pch    = res$pch, col = res$col_main)
        res$plotlegend <- TRUE
      } else {
        res$legend <- list()
        res$plotlegend <- FALSE
      }
    } else {
      if (is.list(dots$legend))
      {
        res$legend <- dots$legend
        res$plotlegend <- TRUE
      } else {
        warning("legend arguement in '...' not parsable")
        res$legend <- list()
        res$plotlegend <- FALSE
      }
    }
  } else {
    res$legend <- list(legend = c("p < 0.001", "p < 0.01", "p < 0.05", "p >= 0.05"),
                       pt.cex = c(2.5 * par()$cex, 1.5 * par()$cex, par()$cex, 0.5 * par()$cex),
                       pch    = res$pch, col = res$col_main)      
    res$plotlegend <- TRUE
  }
  if (any(names(dots) == "plot_legend"))
    res$plotlegend <- dots$plot_legend
      
  
  return(res)
}

.get_legend_position <- function(object, i_var, pars, legend_vals)
{  
  x <-  object[[i_var]][,i_var + 3]
  y1 <- object[[i_var]]$CFE
  y2 <- object[[i_var]][,3]
  arrx1 <- object[[i_var]][,ncol(object[[i_var]])-1]
  arrx2 <- object[[i_var]][,ncol(object[[i_var]])]
  
  y1 <- (y1 - pars[[1]][3]) / (pars[[1]][4] - pars[[1]][3]) * 
        (pars[[2]][4] - pars[[2]][3]) + pars[[2]][3]
  
  y <- c(y1, y2, y1, y1)
  x <- c(x, x, arrx1, arrx2)

  
  yb <- y
  xb <- x
  
  y <- (y -min(y))/(max(y) - min(y))
  x <- (x -min(x))/(max(x) - min(x))
  
  corners <- list(ll = pars[[2]][c(1,3)],
                  ul = pars[[2]][c(1,4)],
                  ur = pars[[2]][c(2,4)],
                  lr = pars[[2]][c(2,3)])
  ## Lower left corner
  c1 <- c(0,0)#pars[[2]][c(1,3)]
  d1 <- sqrt((c1[1] - x)^2 + (c1[2] - y)^2)
  
  ## Upper left corner
  c2 <- c(0,1)#pars[[2]][c(1,4)]
  d2 <- sqrt((c2[1] - x)^2 + (c2[2] - y)^2)
  
  ## Upper right corner
  c3 <- c(1,1)#pars[[2]][c(2,4)]
  d3 <- sqrt((c3[1] - x)^2 + (c3[2] - y)^2)
  
  ## Lower right corner
  c4 <- c(1,0)#pars[[2]][c(2,3)]
  d4 <- sqrt((c4[1] - x)^2 + (c4[2] - y)^2)
  
  minvals <- c(min(d1), min(d2), min(d3), min(d4))
  
  legend_vals$x <- c("bottomleft", "topleft", "topright", "bottomright")[which.max(minvals)]
  legend_vals$plot <- FALSE
  
  ## Test if legend fits
  legend_pos <- do.call("legend", legend_vals)$rect
  
  inside <- apply(matrix(c(xb, yb), ncol = 2), 1, 
                  function(xy, corner)
                  {
                    xy[1] >= corner[1] & xy[1] <= corner[2] & xy[2] >= corner[3] & xy[2] <= corner[4]
                  }, c(legend_pos$left, legend_pos$left + legend_pos$w, legend_pos$top - legend_pos$h, legend_pos$top))
  
  ## If it does not fit, search for best option (the one with the lowest number of points within the rectangle)
  if (any(inside))
  {
    pos <- legend_vals$x
    for (i in c("bottomleft", "topleft", "topright", "bottomright"))
    {
      legend_vals$x <- i
      legend_pos <- do.call("legend", legend_vals)$rect
      inside_2 <- apply(matrix(c(xb, yb), ncol = 2), 1, 
                        function(xy, corner)
                        {
                          xy[1] >= corner[1] & xy[1] <= corner[2] & xy[2] >= corner[3] & xy[2] <= corner[4]
                        }, c(legend_pos$left, legend_pos$left + legend_pos$w, legend_pos$top - legend_pos$h, legend_pos$top))
      if (sum(inside_2) < sum(inside))
        pos <- i
    }
    legend_vals$x <- pos
  }
  legend_vals$plot <- TRUE
  return(legend_vals)
}
  
.formatTable <- function(header, content)
{
  nchar_header <- nchar(header)
  max_char_content <- 1:ncol(content)
  for (i in 1:ncol(content))
  {
    max_char_content[i] <- max(nchar(as.character(content[,i])))
    if (nchar_header[i] < max_char_content[i])
      nchar_header[i] <- max_char_content[i]
  }
  
  table_res <- character(length = nrow(content) + 1)
  table_res[1] <- header[1]
  for (i in 2:ncol(content))
    table_res[1] <- paste(table_res[1], paste(rep.int(" ", nchar_header[i-1] - 
                                                        nchar(header[i-1]) + 3), 
                                              sep = "", collapse = ""), 
                          header[i], sep = "")
  table_res[1] <- paste(table_res[1], "\n", sep = "")
  for (irow in 1:nrow(content))
  {
    table_res[irow + 1] <- as.character(content[irow, 1])
    for (i in 2:ncol(content))
    {
      table_res[irow + 1] <- paste(table_res[irow + 1],
                                   paste(rep.int(" ", nchar_header[i-1] - 
                                                   nchar(as.character(content[irow, i-1])) + 3), 
                                         sep = "", collapse = ""),
                                   as.character(content[irow, i]), sep = "")
    }
    table_res[irow + 1] <- paste(table_res[irow + 1], "\n", sep = "")
  }
  tmp <- max(nchar(table_res[1:2]))
  table_res_cat <- c(paste(c(" ", rep.int("-", tmp), "\n"), 
                           collapse = "", sep = ""),
                     table_res[1],
                     paste(c(rep.int("-", tmp), "\n"), 
                           collapse = "", sep = ""),
                     table_res[-1],
                     paste(c(rep.int("-", tmp), "\n"), 
                           collapse = "", sep = ""))
  
  
  cat(table_res_cat)
  invisible(table_res)
}