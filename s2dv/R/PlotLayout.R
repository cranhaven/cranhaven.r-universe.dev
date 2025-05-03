#'Arrange and Fill Multi-Pannel Layouts With Optional Colour Bar
#'
#'This function takes an array or list of arrays and loops over each of them 
#'to plot all the sub-arrays they contain on an automatically generated 
#'multi-pannel layout. A different plot function (not necessarily from 
#'s2dv) can be applied over each of the provided arrays. The input 
#'dimensions of each of the functions have to be specified, either with the 
#'names or the indices of the corresponding input dimensions. It is possible 
#'to draw a common colour bar at any of the sides of the multi-pannel for all 
#'the s2dv plots that use a colour bar. Common plotting arguments 
#'for all the arrays in 'var' can be specified via the '...' parameter, and 
#'specific plotting arguments for each array can be fully adjusted via 
#''special_args'. It is possible to draw titles for each of the figures, 
#'layout rows, layout columns and for the whole figure. A number of parameters 
#'is provided in order to adjust the position, size and colour of the 
#'components. Blank cells can be forced to appear and later be filled in 
#'manually with customized plots.\cr
#'This function pops up a blank new device and fills it in, so it cannot be 
#'nested in complex layouts.
#'
#'@param fun Plot function (or name of the function) to be called on the 
#'  arrays provided in 'var'. If multiple arrays are provided in 'var', a 
#'  vector of as many function names (character strings!) can be provided in 
#'  'fun', one for each array in 'var'.
#'@param plot_dims Numeric or character string vector with identifiers of the 
#'  input plot dimensions of the plot function specified in 'fun'. If 
#'  character labels are provided, names(dim(var)) or attr('dimensions', var) 
#'  will be checked to locate the dimensions. As many plots as 
#'  prod(dim(var)[-plot_dims]) will be generated. If multiple arrays are 
#'  provided in 'var', 'plot_dims' can be sent a list with a vector of plot 
#'  dimensions for each. If a single vector is provided, it will be used for 
#'  all the arrays in 'var'.
#'@param var Multi-dimensional array with at least the dimensions expected by 
#'  the specified plot function in 'fun'. The dimensions reqired by the 
#'  function must be specified in 'plot_dims'. The dimensions can be 
#'  disordered and will be reordered automatically. Dimensions can optionally 
#'  be labelled in order to refer to them with names in 'plot_dims'. All the 
#'  available plottable sub-arrays will be automatically plotted and arranged 
#'  in consecutive cells of an automatically arranged layout. A list of 
#'  multiple (super-)arrays can be specified. The process will be repeated for 
#'  each of them, by default applying the same plot function to all of them 
#'  or, if properly specified in 'fun', a different plot function will be 
#'  applied to each of them. NAs can be passed to the list: a NA will yield a 
#'  blank cell in the layout, which can be populated after 
#'  (see .SwitchToFigure).
#'@param \dots Parameters to be sent to the plotting function 'fun'. If 
#'  multiple arrays are provided in 'var' and multiple functions are provided 
#'  in 'fun', the parameters provided through \dots will be sent to all the 
#'  plot functions, as common parameters. To specify concrete arguments for 
#'  each of the plot functions see parameter 'special_args'.
#'@param special_args List of sub-lists, each sub-list having specific extra 
#'  arguments for each of the plot functions provided in 'fun'. If you want to 
#'  fix a different value for each plot in the layout you can do so by 
#'  a) splitting your array into a list of sub-arrays (each with the data for 
#'  one plot) and providing it as parameter 'var', 
#'  b) providing a list of named sub-lists in 'special_args', where the names 
#'  of each sub-list match the names of the parameters to be adjusted, and 
#'  each value in a sub-list contains the value of the corresponding parameter.
#'  For example, if the plots are two maps with different arguments, the 
#'  structure would be like:\cr
#'  var:\cr
#'   List of 2\cr
#'    $ : num [1:360, 1:181] 1 3.82 5.02 6.63 8.72 ...\cr
#'    $ : num [1:360, 1:181] 2.27 2.82 4.82 7.7 10.32 ...\cr
#'  special_args:\cr
#'   List of 2\cr
#'   $ :List of 2\cr
#'    ..$ arg1: ...\cr
#'    ..$ arg2: ...\cr
#'   $ :List of 1\cr
#'    ..$ arg1: ...\cr
#'@param nrow Numeric value to force the number of rows in the automatically 
#'  generated layout. If higher than the required, this will yield blank cells 
#'  in the layout (which can then be populated). If lower than the required 
#'  the function will stop. By default it is configured to arrange the layout 
#'  in a shape as square as possible. Blank cells can be manually populated 
#'  after with customized plots (see SwitchTofigure).
#'@param ncol Numeric value to force the number of columns in the 
#'  automatically generated layout. If higher than the required, this will 
#'  yield blank cells in the layout (which can then be populated). If lower 
#'  than the required the function will stop. By default it is configured to 
#'  arrange the layout in a shape as square as possible. Blank cells can be 
#'  manually populated after with customized plots (see SwitchTofigure).
#'@param toptitle Topt title for the multi-pannel. Blank by default.
#'@param row_titles Character string vector with titles for each of the rows 
#'  in the layout. Blank by default.
#'@param col_titles Character string vector with titles for each of the 
#'  columns in the layout. Blank by default.
#'@param bar_scale Scale factor for the common colour bar. Takes 1 by default.
#'@param title_scale Scale factor for the multi-pannel title. Takes 1 by 
#'  default.
#'@param title_margin_scale Scale factor for the margins surrounding the top 
#'  title. Takes 1 by default.
#'@param title_left_shift_scale When plotting row titles, a shift is added 
#'  to the horizontal positioning of the top title in order to center it to 
#'  the region of the figures (without taking row titles into account). This 
#'  shift can be reduced. A value of 0 will remove the shift completely, 
#'  centering the title to the total width of the device. This parameter will 
#'  be disregarded if no 'row_titles' are provided.
#'@param subtitle_scale Scale factor for the row titles and column titles 
#'  (specified in 'row_titles' and 'col_titles'). Takes 1 by default.
#'@param subtitle_margin_scale Scale factor for the margins surrounding the 
#'  subtitles. Takes 1 by default.
#'@param subplot_titles_scale Scale factor for the subplots top titles. Takes 
#'  1 by default.
#'@param units Title at the top of the colour bar, most commonly the units of 
#'  the variable provided in parameter 'var'.
#'@param brks,cols,bar_limits,triangle_ends Usually only providing 'brks' is 
#'  enough to generate the desired colour bar. These parameters allow to 
#'  define n breaks that define n - 1 intervals to classify each of the values 
#'  in 'var'. The corresponding grid cell of a given value in 'var' will be 
#'  coloured in function of the interval it belongs to. These parameters are 
#'  sent to \code{ColorBar()} to generate the breaks and colours. Additional 
#'  colours for values beyond the limits of the colour bar are also generated 
#'  and applied to the plot if 'bar_limits' or 'brks' and 'triangle_ends' are 
#'  properly provided to do so. See ?ColorBar for a full explanation.
#'@param col_inf,col_sup Colour identifiers to colour the values in 'var' that 
#'  go beyond the extremes of the colour bar and to colour NA values, 
#'  respectively. 'colNA' takes 'white' by default. 'col_inf' and 'col_sup' 
#'  will take the value of 'colNA' if not specified. See ?ColorBar for a full 
#'  explanation on 'col_inf' and 'col_sup'.
#'@param color_fun,subsampleg,bar_extra_labels,draw_bar_ticks,draw_separators,triangle_ends_scale,bar_label_digits,bar_label_scale,units_scale,bar_tick_scale,bar_extra_margin Set of parameters to control the visual aspect of the drawn colour bar. See ?ColorBar for a full explanation.
#'@param drawleg Where to draw the common colour bar. Can take values TRUE, 
#'  FALSE or:\cr
#'  'up', 'u', 'U', 'top', 't', 'T', 'north', 'n', 'N'\cr
#'  'down', 'd', 'D', 'bottom', 'b', 'B', 'south', 's', 'S' (default)\cr
#'  'right', 'r', 'R', 'east', 'e', 'E'\cr
#'  'left', 'l', 'L', 'west', 'w', 'W'
#'@param titles Character string vector with titles for each of the figures in 
#'  the multi-pannel, from top-left to bottom-right. Blank by default.
#'@param bar_left_shift_scale When plotting row titles, a shift is added to 
#'  the horizontal positioning of the colour bar in order to center it to the 
#'  region of the figures (without taking row titles into account). This shift 
#'  can be reduced. A value of 0 will remove the shift completely, centering 
#'  the colour bar to the total width of the device. This parameter will be 
#'  disregarded if no 'row_titles' are provided.
#'@param extra_margin Extra margins to be added around the layout, in the 
#'  format c(y1, x1, y2, x2). The units are margin lines. Takes rep(0, 4) 
#'  by default.
#'@param layout_by_rows Logical indicating wether the panels should be filled 
#'  by columns (FALSE) or by raws (TRUE, default).
#'@param fileout File where to save the plot. If not specified (default) a 
#'  graphics device will pop up. Extensions allowed: eps/ps, jpeg, png, pdf, 
#'  bmp and tiff.
#'@param width Width in inches of the multi-pannel. 7 by default, or 11 if 
#'  'fielout' has been specified.
#'@param height Height in inches of the multi-pannel. 7 by default, or 11 if 
#'  'fileout' has been specified.
#'@param size_units Units of the size of the device (file or window) to plot 
#'  in. Inches ('in') by default. See ?Devices and the creator function of 
#'  the corresponding device. 
#'@param res Resolution of the device (file or window) to plot in. See 
#'  ?Devices and the creator function of the corresponding device.
#'@param close_device Whether to close the graphics device after plotting 
#'  the layout and a 'fileout' has been specified. This is useful to avoid 
#'  closing the device when saving the layout into a file and willing to add 
#'  extra elements or figures. Takes TRUE by default. Disregarded if no 
#'  'fileout' has been specified.
#'
#'@return 
#'\item{brks}{
#'  Breaks used for colouring the map (and legend if drawleg = TRUE).
#'}
#'\item{cols}{
#'  Colours used for colouring the map (and legend if drawleg = TRUE). 
#'    Always of length length(brks) - 1.
#'}
#'\item{col_inf}{
#'  Colour used to draw the lower triangle end in the colour bar 
#'    (NULL if not drawn at all).
#'}
#'\item{col_sup}{
#'  Colour used to draw the upper triangle end in the colour bar 
#'    (NULL if not drawn at all).
#'}
#'\item{layout_matrix}{
#'  Underlying matrix of the layout. Useful to later set any of the layout 
#'    cells as current figure to add plot elements. See .SwitchToFigure.
#'}
#'
#'@examples
#'# See examples on Load() to understand the first lines in this example
#'  \dontrun{
#'data_path <- system.file('sample_data', package = 's2dv')
#'expA <- list(name = 'experiment', path = file.path(data_path,
#'             'model/$EXP_NAME$/$STORE_FREQ$_mean/$VAR_NAME$_3hourly',
#'             '$VAR_NAME$_$START_DATE$.nc'))
#'obsX <- list(name = 'observation', path = file.path(data_path,
#'             '$OBS_NAME$/$STORE_FREQ$_mean/$VAR_NAME$',
#'             '$VAR_NAME$_$YEAR$$MONTH$.nc'))
#'
#'# Now we are ready to use Load().
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- Load('tos', list(expA), list(obsX), startDates,
#'                   leadtimemin = 1, leadtimemax = 4, output = 'lonlat',
#'                   latmin = 27, latmax = 48, lonmin = -12, lonmax = 40)
#'  }
#'  \dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                                c('observation'), startDates,
#'                                                leadtimemin = 1,
#'                                                leadtimemax = 4,
#'                                                output = 'lonlat',
#'                                                latmin = 27, latmax = 48,
#'                                                lonmin = -12, lonmax = 40)
#'  }
#'PlotLayout(PlotEquiMap, c('lat', 'lon'), sampleData$mod[1, , 1, 1, , ], 
#'           sampleData$lon, sampleData$lat,
#'           toptitle = 'Predicted tos for Nov 1960 from 1st Nov',
#'           titles = paste('Member', 1:15))
#'
#'@importFrom grDevices dev.cur dev.new dev.off
#'@export
PlotLayout <- function(fun, plot_dims, var, ..., special_args = NULL, 
                       nrow = NULL, ncol = NULL, toptitle = NULL,
                       row_titles = NULL, col_titles = NULL, bar_scale = 1, 
                       title_scale = 1, title_margin_scale = 1,
                       title_left_shift_scale = 1,
                       subtitle_scale = 1, subtitle_margin_scale = 1,
                       subplot_titles_scale = 1,
                       brks = NULL, cols = NULL, drawleg = 'S', titles = NULL, 
                       subsampleg = NULL, bar_limits = NULL, 
                       triangle_ends = NULL, col_inf = NULL, col_sup = NULL,
                       color_fun = clim.colors, 
                       draw_bar_ticks = TRUE, draw_separators = FALSE, 
                       triangle_ends_scale = 1, bar_extra_labels = NULL, 
                       units = NULL, units_scale = 1, bar_label_scale = 1, 
                       bar_tick_scale = 1, bar_extra_margin = rep(0, 4), 
                       bar_left_shift_scale = 1, bar_label_digits = 4, 
                       extra_margin = rep(0, 4), layout_by_rows = TRUE,
                       fileout = NULL, width = NULL, height = NULL, 
                       size_units = 'in', res = 100, close_device = TRUE) {
  # If there is any filenames to store the graphics, process them
  # to select the right device
  if (!is.null(fileout)) {
    deviceInfo <- .SelectDevice(fileout = fileout, width = width, height = height, units = size_units, res = res)
    saveToFile <- deviceInfo$fun
    fileout <- deviceInfo$files
  }

  is_single_na <- function(x) ifelse(length(x) > 1, FALSE, is.na(x))
  # Check var
  if (!is.list(var) & (is.array(var) || (is_single_na(var)))) {
    var <- list(var)
  } else if (is.list(var)) {
    if (!all(sapply(var, is.array) | sapply(var, is_single_na))) {
      stop("Parameter 'var' must be an array or a list of arrays (or NA values).")
    }
  } else {
    stop("Parameter 'var' must be an array or a list of arrays.")
  }

  # Check fun
  if (length(fun) == 1) {
    if (is.function(fun)) {
      fun <- as.character(substitute(fun))
    }
    if (is.character(fun)) {
      fun <- rep(fun, length(var))
    }
  }
  if (!is.character(fun) || (length(fun) != length(var))) {
    stop("Parameter 'fun' must be a single function or a vector of function names, one for each array provided in parameter 'var'.")
  }

  # Check special_args
  if (!is.null(special_args)) {
    if (!is.list(special_args) || any(!sapply(special_args, is.list))) {
      stop("Parameter 'special_args' must be a list of lists.")
    } else if (length(special_args) != length(var)) {
      stop("Parameter 'special_args' must contain a list of special arguments for each array provided in 'var'.")
    }
  }

  # Check plot_dims
  if (is.character(plot_dims) || is.numeric(plot_dims)) {
    plot_dims <- replicate(length(var), plot_dims, simplify = FALSE)
  }
  if (!is.list(plot_dims) || !all(sapply(plot_dims, is.character) | sapply(plot_dims, is.numeric)) ||
      (length(plot_dims) != length(var))) {
    stop("Parameter 'plot_dims' must contain a single numeric or character vector with dimension identifiers or a vector for each array provided in parameter 'var'.")
  }

  # Check nrow
  if (!is.null(nrow)) {
    if (!is.numeric(nrow)) {
      stop("Parameter 'nrow' must be numeric or NULL.")
    }
    nrow <- round(nrow)
  }

  # Check ncol
  if (!is.null(ncol)) {
    if (!is.numeric(ncol)) {
      stop("Parameter 'ncol' must be numeric or NULL.")
    }
    ncol <- round(ncol)
  }
  # Check layout_by_rows
  if (!is.logical(layout_by_rows)) {
     stop("Parameter 'layout_by_rows' must be logical.")
  }

  # Check toptitle
  if (is.null(toptitle) || is.na(toptitle)) {
    toptitle <- ''
  }
  if (!is.character(toptitle)) {
    stop("Parameter 'toptitle' must be a character string.")
  }

  # Check row_titles
  if (!is.null(row_titles)) {
    if (!is.character(row_titles)) {
      stop("Parameter 'row_titles' must be a vector of character strings.")
    }
  }

  # Check col_titles
  if (!is.null(row_titles)) {
    if (!is.character(row_titles)) {
      stop("Parameter 'row_titles' must be a vector of character strings.")
    }
  }

  # Check drawleg
  if (is.character(drawleg)) {
    if (drawleg %in% c('up', 'u', 'U', 'top', 't', 'T', 'north', 'n', 'N')) {
      drawleg <- 'N'
    } else if (drawleg %in% c('down', 'd', 'D', 'bottom', 'b', 'B', 'south', 's', 'S')) {
      drawleg <- 'S'
    } else if (drawleg %in% c('right', 'r', 'R', 'east', 'e', 'E')) {
      drawleg <- 'E'
    } else if (drawleg %in% c('left', 'l', 'L', 'west', 'w', 'W')) {
      drawleg <- 'W'
    } else {
      stop("Parameter 'drawleg' must be either TRUE, FALSE or a valid identifier of a position (see ?PlotMultiMap).")
    }
  } else if (!is.logical(drawleg)) {
    stop("Parameter 'drawleg' must be either TRUE, FALSE or a valid identifier of a position (see ?PlotMultiMap).")
  }
  if (drawleg != FALSE && all(sapply(var, is_single_na)) && 
      (is.null(brks) || length(brks) < 2)) {
    stop("Either data arrays in 'var' or breaks in 'brks' must be provided if 'drawleg' is requested.")
  }

  # Check the rest of parameters (unless the user simply wants to build an empty layout)
  if (!all(sapply(var, is_single_na))) {
    if (!all(is.na(unlist(var)))) {
      tmp <- !is.infinite(unlist(var))
      var_limits <- c(min(unlist(var)[tmp], na.rm = TRUE),
                      max(unlist(var)[tmp], na.rm = TRUE))
    } else {
      if (!is.null(brks)) {
        #NOTE: var_limits be like this to avoid warnings from ColorBar
        var_limits <- c(min(brks, na.rm = TRUE) + diff(brks)[1],
                        max(brks, na.rm = TRUE))
      } else if (!is.null(bar_limits)) {
        var_limits <- c(bar_limits[1] + 0.01, bar_limits[2])
      } else {
        var_limits <- c(-0.5, 0.5) # random range since colorbar is not going to be plotted
        if (!isFALSE(drawleg)) {
          drawleg <- FALSE
          .warning("All data are NAs. Color bar won't be drawn. If you want to have color bar still, define parameter 'brks' or 'bar_limits'.")
        }
      }
    }
  }

  colorbar <- ColorBar(brks, cols, FALSE, subsampleg, bar_limits,
                       var_limits, triangle_ends, col_inf, col_sup, color_fun, 
                       plot = FALSE, draw_bar_ticks, 
                       draw_separators, triangle_ends_scale, bar_extra_labels,
                       units, units_scale, bar_label_scale, bar_tick_scale,
                       bar_extra_margin, bar_label_digits)
 
  # Check bar_scale
  if (!is.numeric(bar_scale)) {
    stop("Parameter 'bar_scale' must be numeric.")
  }

  # Check bar_left_shift_scale
  if (!is.numeric(bar_left_shift_scale)) {
    stop("Parameter 'bar_left_shift_scale' must be numeric.")
  }

  # Check title_scale
  if (!is.numeric(title_scale)) {
    stop("Parameter 'title_scale' must be numeric.")
  }

  # Check title_margin_scale
  if (!is.numeric(title_margin_scale)) {
    stop("Parameter 'title_margin_scale' must be numeric.")
  }

  # Check title_left_shift_scale
  if (!is.numeric(title_left_shift_scale)) {
    stop("Parameter 'title_left_shift_scale' must be numeric.")
  }

  # Check subtitle_scale
  if (!is.numeric(subtitle_scale)) {
    stop("Parameter 'subtite_scale' must be numeric.")
  }

  # Check subtitle_margin_scale
  if (!is.numeric(subtitle_margin_scale)) {
    stop("Parameter 'subtite_margin_scale' must be numeric.")
  }

  # Check subplot_titles_scale
  if (!is.numeric(subplot_titles_scale)) {
    stop("Parameter 'subplot_titles_scale' must be numeric.")
  }

  # Check titles
  if (!all(sapply(titles, is.character))) {
    stop("Parameter 'titles' must be a vector of character strings.")
  }

  # Check extra_margin
  if (!is.numeric(extra_margin) || length(extra_margin) != 4) {
    stop("Parameter 'extra_margin' must be a numeric vector with 4 elements.")
  }

  # Check width
  if (is.null(width)) {
    if (is.null(fileout)) {
      width <- 7
    } else {
      width <- 11
    }
  }
  if (!is.numeric(width)) {
    stop("Parameter 'width' must be numeric.")
  }

  # Check height
  if (is.null(height)) {
    if (is.null(fileout)) {
      height <- 7
    } else {
      height <- 8
    }
  }
  if (!is.numeric(height)) {
    stop("Parameter 'height' must be numeric.")
  }

  # Check close_device
  if (!is.logical(close_device)) {
    stop("Parameter 'close_device' must be logical.")
  }

  # Count the total number of maps and reorder each array of maps to have the lat and lon dimensions at the end.
  n_plots <- 0
  plot_array_i <- 1
  for (plot_array in var) {
    if (is_single_na(plot_array)) {
      n_plots <- n_plots + 1
    } else {
      dim_ids <- plot_dims[[plot_array_i]]
      if (is.character(dim_ids)) {
        dimnames <- NULL
        if (!is.null(names(dim(plot_array)))) {
          dimnames <- names(dim(plot_array))
        } else if (!is.null(attr(plot_array, 'dimensions'))) {
          dimnames <- attr(plot_array, 'dimensions')
        }
        if (!is.null(dimnames)) {
          if (any(!sapply(dim_ids, `%in%`, dimnames))) {
            stop("All arrays provided in parameter 'var' must have all the dimensions in 'plot_dims'.")
          }
          dim_ids <- sapply(dim_ids, function(x) which(dimnames == x)[1])
          var[[plot_array_i]] <- Reorder(var[[plot_array_i]], c((1:length(dim(plot_array)))[-dim_ids], dim_ids))
        } else {
          .warning(paste0("Assuming the ", plot_array_i, "th array provided in 'var' has 'plot_dims' as last dimensions (right-most)."))
          dims <- tail(c(rep(1, length(dim_ids)), dim(plot_array)), length(dim_ids))
          dim_ids <- tail(1:length(dim(plot_array)), length(dim_ids))
          if (length(dim(var[[plot_array_i]])) < length(dims)) {
            dim(var[[plot_array_i]]) <- dims
          }
        }
      } else if (any(dim_ids > length(dim(plot_array)))) {
        stop("Parameter 'plot_dims' contains dimension identifiers out of range.")
      }
      n_plots <- n_plots + prod(dim(plot_array)[-dim_ids])
      #n_plots <- n_plots + prod(head(c(rep(1, length(dim_ids)), dim(plot_array)), length(dim(plot_array))))
      if (length(dim(var[[plot_array_i]])) == length(dim_ids)) {
        dim(var[[plot_array_i]]) <- c(1, dim(var[[plot_array_i]]))
        dim_ids <- dim_ids + 1
      }
      plot_dims[[plot_array_i]] <- dim_ids
    }
    plot_array_i <- plot_array_i + 1
  }
  if (is.null(nrow) && is.null(ncol)) {
    ncol <- ceiling(sqrt(n_plots))
    nrow <- ceiling(n_plots/ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(n_plots/nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n_plots/ncol)
  } else if (nrow * ncol < n_plots) {
    stop("There are more arrays to plot in 'var' than cells defined by 'nrow' x 'ncol'.")
  }

  if (is.logical(drawleg) && drawleg) {
    if (nrow > ncol) {
      drawleg <- 'S'
    } else {
      drawleg <- 'E'
    }
  }
  vertical <- drawleg %in% c('E', 'W')

  # Open connection to graphical device
  if (!is.null(fileout)) {
    saveToFile(fileout)
  } else if (names(dev.cur()) == 'null device') {
    dev.new(units = size_units, res = res, width = width, height = height)
  } else if (prod(par('mfrow')) > 1) {
    dev.new(units = units, res = res, width = width, height = height)
  }

  # Take size of device and set up layout:
  # ---------------------------------------------
  # |0000000000000000000000000000000000000000000|
  # |0000000000000000 TOP TITLE 0000000000000000|
  # |0000000000000000000000000000000000000000000|
  # |-------------------------------------------|
  # |00000|0000000000000000000000000000000000000|
  # |00000|000000000000 ROW TITLES 0000000000000|
  # |00000|0000000000000000000000000000000000000|
  # |00000|-------------------------------------|
  # |0   0|222222222222222222|333333333333333333|
  # |0 C 0|222222222222222222|333333333333333333|
  # |0 O 0|222222222222222222|333333333333333333|
  # |0 L 0|2222 FIGURE 1 2222|3333 FIGURE 2 3333|
  # |0   0|222222222222222222|333333333333333333|
  # |0 T 0|222222222222222222|333333333333333333|
  # |0 I 0|222222222222222222|333333333333333333|
  # |0 T 0|-------------------------------------|
  # |0 L 0|444444444444444444|555555555555555555|
  # |0 S 0|444444444444444444|555555555555555555|
  # |0   0|444444444444444444|555555555555555555|
  # |00000|4444 FIGURE 3 4444|5555 FIGURE 4 5555|
  # |00000|444444444444444444|555555555555555555|
  # |00000|444444444444444444|555555555555555555|
  # |00000|444444444444444444|555555555555555555|
  # |-------------------------------------------|
  # |1111111111111111111111111111111111111111111|
  # |1111111111111111 COLOR BAR 1111111111111111|
  # |1111111111111111111111111111111111111111111|
  # ---------------------------------------------
  device_size <- par('din')
  device_size[1] <- device_size[1] - sum(extra_margin[c(2, 4)])
  device_size[2] <- device_size[2] - sum(extra_margin[c(1, 3)])
  cs <- char_size <- par('csi')
  title_cex <- 2.5 * title_scale
  title_margin <- 0.5 * title_cex * title_margin_scale
  subtitle_cex <- 1.5 * subtitle_scale
  subtitle_margin <- 0.5 * sqrt(nrow * ncol) * subtitle_cex * subtitle_margin_scale
  mat_layout <- 1:(nrow * ncol)
  if (drawleg != FALSE) {
    if (all(fun %in% 'PlotMostLikelyQuantileMap')) {  #multi_colorbar
      multi_colorbar <- TRUE
      cat_dim <- list(...)$cat_dim
      if (is.null(cat_dim)) cat_dim <- 'bin'  # default
      nmap <- as.numeric(dim(var[[1]])[cat_dim])
      minimum_value <- ceiling(1 / nmap * 10 * 1.1) * 10
      display_range = c(minimum_value, 100)
      mat_layout <- mat_layout + nmap
    } else {
      multi_colorbar <- FALSE
      mat_layout <- mat_layout + 1
    }
  }
  mat_layout <- matrix(mat_layout, nrow, ncol, byrow = layout_by_rows)
  fsu <- figure_size_units <- 10  # unitless
  widths <- rep(fsu, ncol)
  heights <- rep(fsu, nrow)
   # Useless
#  n_figures <- nrow * ncol

  if (drawleg != FALSE) {
    if (drawleg == 'N') {
      mat_layout <- rbind(rep(1, dim(mat_layout)[2]), mat_layout)
      heights <- c(round(bar_scale * 2 * nrow), heights)
    } else if (drawleg == 'S') {
      if (multi_colorbar) {
        new_mat_layout <- c()
        for (i_col in 1:ncol) {
          new_mat_layout <- c(new_mat_layout, rep(mat_layout[, i_col], nmap))
        }
        new_mat_layout <- matrix(new_mat_layout, nrow, nmap * ncol)
        colorbar_row <- rep(1:nmap, each = ncol)
        mat_layout <- rbind(new_mat_layout, as.numeric(colorbar_row))
        widths <- rep(widths, nmap)
      } else {
        mat_layout <- rbind(mat_layout, rep(1, dim(mat_layout)[2]))
      }
      heights <- c(heights, round(bar_scale * 2 * nrow))
    } else if (drawleg == 'W') {
      mat_layout <- cbind(rep(1, dim(mat_layout)[1]), mat_layout)
      widths <- c(round(bar_scale * 3 * ncol), widths)
    } else if (drawleg == 'E') {
      mat_layout <- cbind(mat_layout, rep(1, dim(mat_layout)[1]))
      widths <- c(widths, round(bar_scale * 3 * ncol))
    }
   # Useless
#    n_figures <- n_figures + 1
  }

  # row and col titles
  if (length(row_titles) > 0) {
    mat_layout <- cbind(rep(0, dim(mat_layout)[1]), mat_layout)
    widths <- c(((subtitle_cex + subtitle_margin / 2) * cs / device_size[1]) * ncol * fsu, widths)
  }
  if (length(col_titles) > 0) {
    mat_layout <- rbind(rep(0, dim(mat_layout)[2]), mat_layout)
    heights <- c(((subtitle_cex + subtitle_margin) * cs / device_size[2]) * nrow * fsu, heights)
  }
  # toptitle
  if (toptitle != '') {
    mat_layout <- rbind(rep(0, dim(mat_layout)[2]), mat_layout)
    heights <-  c(((title_cex + title_margin) * cs / device_size[2]) * nrow * fsu, heights)
  }
  par(oma = extra_margin)
  layout(mat_layout, widths, heights)
  # Draw the color bar
  if (drawleg != FALSE) {
    if (length(row_titles) > 0) {
      bar_extra_margin[2] <- bar_extra_margin[2] + (subtitle_cex + subtitle_margin / 2) * 
                                                   bar_left_shift_scale
    }

    if (multi_colorbar) {  # multiple colorbar
      if (!is.null(list(...)$bar_titles)) {
        bar_titles <- list(...)$bar_titles
      } else {
        bar_titles <- NULL
      }
      GradientCatsColorBar(nmap = nmap, 
                           brks = brks, cols = cols, vertical = vertical, subsampleg = subsampleg,
                           bar_limits = display_range, var_limits = var_limits,
                           triangle_ends = triangle_ends, plot = TRUE,
                           draw_separators = draw_separators,
                           bar_titles = bar_titles, title_scale = units_scale,
                           label_scale = bar_label_scale, extra_margin = bar_extra_margin)

    } else {  # one colorbar
      ColorBar(brks = colorbar$brks, cols = colorbar$cols, vertical = vertical, subsampleg = subsampleg,
               bar_limits = bar_limits, var_limits = var_limits,
               triangle_ends = triangle_ends, col_inf = colorbar$col_inf,
               col_sup = colorbar$col_sup, color_fun = color_fun, plot = TRUE, draw_ticks = draw_bar_ticks,
               draw_separators = draw_separators, triangle_ends_scale = triangle_ends_scale,
               extra_labels = bar_extra_labels,
               title = units, title_scale = units_scale, label_scale = bar_label_scale, tick_scale = bar_tick_scale,
               extra_margin = bar_extra_margin, label_digits = bar_label_digits)

    }
  }

  # Draw titles
  if (toptitle != '' || length(col_titles) > 0 || length(row_titles) > 0) {
    plot(0, type = 'n', ann = FALSE, axes = FALSE, xaxs = 'i', yaxs = 'i', 
         xlim = c(0, 1), ylim = c(0, 1))
    width_lines <- par('fin')[1] / par('csi')
    plot_lines <- par('pin')[1] / par('csi')
    plot_range <- par('xaxp')[2] - par('xaxp')[1]
    size_units_per_line <- plot_range / plot_lines
    if (toptitle != '') {
      title_x_center <- par('xaxp')[1] - par('mar')[2] * size_units_per_line + 
                        ncol * width_lines * size_units_per_line / 2
      if (length(row_titles) > 0) {
        title_x_center <- title_x_center - (1 - title_left_shift_scale) * 
                          (subtitle_cex + subtitle_margin) / 2 * size_units_per_line
      }
      title_y_center <- par('mar')[3] + (title_margin + title_cex) / 2
      if (length(col_titles > 0)) {
        title_y_center <- title_y_center + (subtitle_margin + subtitle_cex)
      }
      mtext(toptitle, cex = title_cex, line = title_y_center, at = title_x_center,
            padj = 0.5)
    }
    if (length(col_titles) > 0) {
      t_x_center <- par('xaxp')[1] - par('mar')[2] * size_units_per_line
      for (t in 1:ncol) {
        mtext(col_titles[t], cex = subtitle_cex,
              line = par('mar')[3] + (subtitle_margin + subtitle_cex) / 2,
              at = t_x_center + (t - 0.5) * width_lines * size_units_per_line,
              padj = 0.5)
      }
    }
    height_lines <- par('fin')[2] / par('csi')
    plot_lines <- par('pin')[2] / par('csi')
    plot_range <- par('yaxp')[2] - par('yaxp')[1]
    size_units_per_line <- plot_range / plot_lines
    if (length(row_titles) > 0) {
      t_y_center <- par('yaxp')[1] - par('mar')[1] * size_units_per_line
      for (t in 1:nrow) {
        mtext(row_titles[t], cex = subtitle_cex,
              line = par('mar')[2] + (subtitle_margin + subtitle_cex) / 2, 
              at = t_y_center - (t - 1.5) * height_lines * size_units_per_line, 
              padj = 0.5, side = 2)
      }
    }
    par(new = TRUE)
  }

  array_number <- 1
  plot_number <- 1
  # For each array provided in var
  lapply(var, function(x) {
    if (is_single_na(x)) {
      if (!all(sapply(var[array_number:length(var)], is_single_na))) {
        plot.new()
        par(new = FALSE)
      }
      plot_number <<- plot_number + 1
    } else {
      if (is.character(plot_dims[[array_number]])) {
        plot_dim_indices <- which(names(dim(x)) %in% plot_dims[[array_number]])
      } else {
        plot_dim_indices <- plot_dims[[array_number]]
      }
      # For each of the arrays provided in that array
      apply(x, (1:length(dim(x)))[-plot_dim_indices], 
            function(y) {
        # Do the plot. colorbar is not drew.
        fun_args <- c(list(y, toptitle = titles[plot_number], drawleg = FALSE), list(...),
                      special_args[[array_number]])
#        funct <- fun[[array_number]]
        if (fun[[array_number]] %in% c('PlotEquiMap', 'PlotStereoMap')) {
          fun_args <- c(fun_args, list(brks = colorbar$brks, cols = colorbar$cols, 
                                       col_inf = colorbar$col_inf, 
                                       col_sup = colorbar$col_sup, 
                                       title_scale = subplot_titles_scale  # when all the functions have this argument, put it above in fun_args
                                       ))
        } else if (fun[[array_number]] == c('PlotSection')) {
          fun_args <- c(fun_args, list(brks = colorbar$brks, cols = colorbar$cols))

        } else if (fun[[array_number]] %in% 'PlotMostLikelyQuantileMap') {
          #TODO: pre-generate colorbar params? like above
          fun_args <- c(fun_args, list(brks = brks, cols = cols))
        }
        do.call(fun[[array_number]], fun_args)
        plot_number <<- plot_number + 1
      })
    }
    array_number <<- array_number + 1
  })

  # If the graphic was saved to file, close the connection with the device
  if (!is.null(fileout) && close_device) dev.off()

  invisible(list(brks = colorbar$brks, cols = colorbar$cols, 
                 col_inf = colorbar$col_inf, col_sup = colorbar$col_sup,
                 layout_matrix = mat_layout))
}
