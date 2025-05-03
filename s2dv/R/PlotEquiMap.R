#'Maps A Two-Dimensional Variable On A Cylindrical Equidistant Projection
#'
#'Map longitude-latitude array (on a regular rectangular or gaussian grid) 
#'on a cylindrical equidistant latitude and longitude projection with coloured 
#'grid cells. Only the region for which data has been provided is displayed. 
#'A colour bar (legend) can be plotted and adjusted. It is possible to draw 
#'superimposed arrows, dots, symbols, contour lines and boxes. A number of 
#'options is provided to adjust the position, size and colour of the 
#'components. Some parameters are provided to add and adjust the masks that
#'include continents, oceans, and lakes. This plot function is compatible with
#'figure layouts if colour bar is disabled.
#'
#'@param var Array with the values at each cell of a grid on a regular 
#'  rectangular or gaussian grid. The array is expected to have two 
#'  dimensions: c(latitude, longitude). Longitudes can be in ascending or 
#'  descending order and latitudes in any order. It can contain NA values 
#'  (coloured with 'colNA'). Arrays with dimensions c(longitude, latitude) 
#'  will also be accepted but 'lon' and 'lat' will be used to disambiguate so 
#'  this alternative is not appropriate for square arrays. It is allowed that
#'  the positions of the longitudinal and latitudinal coordinate dimensions 
#'  are interchanged.
#'@param lon Numeric vector of longitude locations of the cell centers of the 
#'  grid of 'var', in ascending or descending order (same as 'var'). Expected 
#'  to be regularly spaced, within either of the ranges [-180, 180] or 
#'  [0, 360]. Data for two adjacent regions split by the limits of the 
#'  longitude range can also be provided, e.g. \code{lon = c(0:50, 300:360)} 
#'  ('var' must be provided consitently).
#'@param lat Numeric vector of latitude locations of the cell centers of the 
#'  grid of 'var', in any order (same as 'var'). Expected to be from a regular 
#'  rectangular or gaussian grid, within the range [-90, 90].
#'@param varu Array of the zonal component of wind/current/other field with 
#'  the same dimensions as 'var'. It is allowed that the positions of the 
#'  longitudinal and latitudinal coordinate dimensions are interchanged.
#'@param varv Array of the meridional component of wind/current/other field 
#'  with the same dimensions as 'var'. It is allowed that the positions of the 
#'  longitudinal and latitudinal coordinate dimensions are interchanged.
#'@param toptitle Top title of the figure, scalable with parameter 
#'  'title_scale'.
#'@param sizetit Scale factor for the figure top title provided in parameter 
#'  'toptitle'. Deprecated. Use 'title_scale' instead.
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
#'@param col_inf,col_sup,colNA Colour identifiers to colour the values in 
#'  'var' that go beyond the extremes of the colour bar and to colour NA 
#'  values, respectively. 'colNA' takes attr(cols, 'na_color') if available by 
#'  default, where cols is the parameter 'cols' if provided or the vector of 
#'  colors returned by 'color_fun'. If not available, it takes 'pink' by 
#'  default. 'col_inf' and 'col_sup' will take the value of 'colNA' if not 
#'  specified. See ?ColorBar for a full explanation on 'col_inf' and 'col_sup'.
#'@param color_fun,subsampleg,bar_extra_labels,draw_bar_ticks Set of 
#'  parameters to control the visual aspect of the drawn colour bar 
#'  (1/3). See ?ColorBar for a full explanation.
#'@param draw_separators,triangle_ends_scale,bar_label_digits Set of 
#'  parameters to control the visual aspect of the drawn colour bar 
#'  (2/3). See ?ColorBar for a full explanation.
#'@param bar_label_scale,units_scale,bar_tick_scale,bar_extra_margin Set of  
#'  parameters to control the visual aspect of the drawn colour bar (3/3). 
#'  See ?ColorBar for a full explanation.
#'@param square Logical value to choose either to draw a coloured square for 
#'  each grid cell in 'var' (TRUE; default) or to draw contour lines and fill 
#'  the spaces in between with colours (FALSE). In the latter case, 
#'  'filled.continents' will take the value FALSE if not specified.
#'@param filled.continents Colour to fill in drawn projected continents. 
#'  Takes the value gray(0.5) by default or, if 'square = FALSE', takes the 
#'  value FALSE. If set to FALSE, continents are not filled in.
#'@param filled.oceans A logical value or the color name to fill in drawn 
#'  projected oceans. The default value is FALSE. If it is TRUE, the default
#'  colour is "light blue".
#'@param country.borders A logical value indicating if the country borders 
#'  should be plotted (TRUE) or not (FALSE). It only works when 
#'  'filled.continents' is FALSE. The default value is FALSE.
#'@param coast_color Colour of the coast line of the drawn projected continents.
#'   Takes the value gray(0.5) by default.
#'@param coast_width Line width of the coast line of the drawn projected 
#'  continents. Takes the value 1 by default.
#'@param lake_color Colour of the lake or other water body inside continents.
#'  The default value is NULL.
#'@param shapefile A character string of the path to a .rds file or a list 
#'  object containinig shape file data. If it is a .rds file, it should contain
#'  a list. The list should contains 'x' and 'y' at least, which indicate the 
#'  location of the shape. The default value is NULL.
#'@param shapefile_color Line color of the shapefile.
#'@param shapefile_lwd Line width of the shapefile. The default value is 1. 
#'@param contours Array of same dimensions as 'var' to be added to the plot 
#'  and displayed with contours. Parameter 'brks2' is required to define the 
#'  magnitude breaks for each contour curve. Disregarded if 'square = FALSE'.
#'  It is allowed that the positions of the longitudinal and latitudinal 
#'  coordinate dimensions are interchanged.
#'@param brks2 Vector of magnitude breaks where to draw contour curves for the 
#'  array provided in 'contours' or if 'square = FALSE'.
#'@param contour_lwd Line width of the contour curves provided via 'contours' 
#'  and 'brks2', or if 'square = FALSE'.
#'@param contour_color Line color of the contour curves provided via 'contours' 
#'  and 'brks2', or if 'square = FALSE'.
#'@param contour_lty Line type of the contour curves. Takes 1 (solid) by 
#'  default. See help on 'lty' in par() for other accepted values.
#'@param contour_draw_label A logical value indicating whether to draw the 
#'  contour labels or not. The default value is TRUE.
#'@param contour_label_scale Scale factor for the superimposed labels when 
#'  drawing contour levels.
#'@param dots Array of same dimensions as 'var' or with dimensions 
#'  c(n, dim(var)), where n is the number of dot/symbol layers to add to the 
#'  plot. A value of TRUE at a grid cell will draw a dot/symbol on the 
#'  corresponding square of the plot. By default all layers provided in 'dots' 
#'  are plotted with dots, but a symbol can be specified for each of the 
#'  layers via the parameter 'dot_symbol'. It is allowed that the positions of
#'  the longitudinal and latitudinal coordinate dimensions are interchanged.
#'@param dot_symbol Single character/number or vector of characters/numbers 
#'  that correspond to each of the symbol layers specified in parameter 'dots'. 
#'  If a single value is specified, it will be applied to all the layers in 
#'  'dots'. Takes 15 (centered square) by default. See 'pch' in par() for 
#'  additional accepted options.
#'@param dot_size Scale factor for the dots/symbols to be plotted, specified 
#'  in 'dots'. If a single value is specified, it will be applied to all 
#'  layers in 'dots'. Takes 1 by default.
#'@param arr_subsamp Subsampling factor to select a subset of arrows in 
#'  'varu' and 'varv' to be drawn. Only one out of arr_subsamp arrows will 
#'  be drawn. Takes 1 by default.
#'@param arr_scale Scale factor for drawn arrows from 'varu' and 'varv'. 
#'  Takes 1 by default.
#'@param arr_ref_len Length of the refence arrow to be drawn as legend at the 
#'  bottom of the figure (in same units as 'varu' and 'varv', only affects the
#'  legend for the wind or variable in these arrays). Defaults to 15.
#'@param arr_units Units of 'varu' and 'varv', to be drawn in the legend. 
#'  Takes 'm/s' by default.
#'@param arr_scale_shaft Parameter for the scale of the shaft of the arrows 
#'  (which also depend on the number of figures and the arr_scale parameter). 
#'  Defaults to 1.
#'@param arr_scale_shaft_angle Parameter for the scale of the angle of the 
#'  shaft of the arrows (which also depend on the number of figure and the 
#'  arr_scale parameter). Defaults to 1.
#'@param axelab Whether to draw longitude and latitude axes or not. 
#'  TRUE by default.
#'@param labW Whether to label the longitude axis with a 'W' instead of minus 
#'  for negative values. Defaults to FALSE.
#'@param lab_dist_x A numeric of the distance of the longitude labels to the 
#'  box borders. The default value is NULL and is automatically adjusted by 
#'  the function.
#'@param lab_dist_y A numeric of the distance of the latitude labels to the 
#'  box borders. The default value is NULL and is automatically adjusted by 
#'  the function.
#'@param degree_sym A logical indicating whether to include degree symbol 
#'  (30° N) or not (30N; default).
#'@param intylat Interval between latitude ticks on y-axis, in degrees. 
#'  Defaults to 20.
#'@param intxlon Interval between latitude ticks on x-axis, in degrees. 
#'  Defaults to 20.
#'@param xlonshft A numeric of the degrees to shift the latitude ticks. The 
#'  default value is 0.
#'@param ylatshft A numeric of the degrees to shift the longitude ticks. The
#'  default value is 0.
#'@param xlabels A vector of character string of the custumized x-axis labels.
#'  The values should correspond to each tick, which is decided by the longitude
#'  and parameter 'intxlon'. The default value is NULL and the labels will be
#'  automatically generated.
#'@param ylabels A vector of character string of the custumized y-axis labels.
#'  The values should correspond to each tick, which is decided by the latitude
#'  and parameter 'intylat'. The default value is NULL and the labels will be
#'  automatically generated.
#'@param axes_tick_scale Scale factor for the tick lines along the longitude 
#'  and latitude axes.
#'@param axes_label_scale Scale factor for the labels along the longitude 
#'  and latitude axes.
#'@param drawleg Whether to plot a color bar (legend, key) or not. Defaults to 
#'  TRUE. It is not possible to plot the colour bar if 'add = TRUE'. Use 
#'  ColorBar() and the return values of PlotEquiMap() instead.
#'@param boxlim Limits of a box to be added to the plot, in degrees: 
#'  c(x1, y1, x2, y2). A list with multiple box specifications can also be 
#'  provided.
#'@param boxcol Colour of the box lines. A vector with a colour for each of 
#'  the boxes is also accepted. Defaults to 'purple2'.
#'@param boxlwd Line width of the box lines. A vector with a line width for 
#'  each of the boxes is also accepted. Defaults to 5.
#'@param margin_scale Scale factor for the margins around the map plot, with 
#'  the format c(y1, x1, y2, x2). Defaults to rep(1, 4). If drawleg = TRUE, 
#'  then margin_scale[1] is subtracted 1 unit.
#'@param title_scale Scale factor for the figure top title. Defaults to 1.
#'@param numbfig Number of figures in the layout the plot will be put into. 
#'  A higher numbfig will result in narrower margins and smaller labels, 
#'  axe labels, ticks, thinner lines, ... Defaults to 1.
#'@param fileout File where to save the plot. If not specified (default) a 
#'  graphics device will pop up. Extensions allowed: eps/ps, jpeg, png, pdf, 
#'  bmp and tiff.
#'@param width File width, in the units specified in the parameter size_units 
#'  (inches by default). Takes 8 by default.
#'@param height File height, in the units specified in the parameter 
#'  size_units (inches by default). Takes 5 by default.
#'@param size_units Units of the size of the device (file or window) to plot 
#'  in. Inches ('in') by default. See ?Devices and the creator function of 
#'  the corresponding device.
#'@param res Resolution of the device (file or window) to plot in. See 
#'  ?Devices and the creator function of the corresponding device.
#'@param \dots Arguments to be passed to the method. Only accepts the following 
#'  graphical parameters:\cr
#'  adj ann ask bg bty cex.sub cin col.axis col.lab col.main col.sub cra crt 
#'  csi cxy err family fg font font.axis font.lab font.main font.sub lend 
#'  lheight ljoin lmitre mex mfcol mfrow mfg mkh omd omi page pch pin plt 
#'  pty smo srt tcl usr xaxp xaxs xaxt xlog xpd yaxp yaxs yaxt ylbias ylog \cr
#'  For more information about the parameters see `par`.
#'
#'@return 
#'\item{brks}{
#'  Breaks used for colouring the map (and legend if drawleg = TRUE).
#'}
#'\item{cols}{
#'  Colours used for colouring the map (and legend if drawleg = TRUE). Always 
#'    of length length(brks) - 1.
#'}
#'\item{col_inf}{
#'  Colour used to draw the lower triangle end in the colour bar (NULL if not 
#'   drawn at all).
#' }
#'\item{col_sup}{
#'  Colour used to draw the upper triangle end in the colour bar (NULL if not 
#'   drawn at all).
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
#'PlotEquiMap(sampleData$mod[1, 1, 1, 1, , ], sampleData$lon, sampleData$lat, 
#'            toptitle = 'Predicted sea surface temperature for Nov 1960 from 1st Nov',
#'            title_scale = 0.5)
#'@import graphics maps
#'@importFrom grDevices dev.cur dev.new dev.off gray
#'@importFrom stats cor
#'@export
PlotEquiMap <- function(var, lon, lat, varu = NULL, varv = NULL,
                        toptitle = NULL, sizetit = NULL, units = NULL, 
                        brks = NULL, cols = NULL, bar_limits = NULL, 
                        triangle_ends = NULL, col_inf = NULL, col_sup = NULL, 
                        colNA = NULL, color_fun = clim.palette(),
                        square = TRUE, filled.continents = NULL,
                        filled.oceans = FALSE, country.borders = FALSE,
                        coast_color = NULL, coast_width = 1, lake_color = NULL,
                        shapefile = NULL, shapefile_color = NULL, shapefile_lwd = 1,
                        contours = NULL, brks2 = NULL, contour_lwd = 0.5,
                        contour_color = 'black', contour_lty = 1,
                        contour_draw_label = TRUE, contour_label_scale = 1,
                        dots = NULL, dot_symbol = 4, dot_size = 1, 
                        arr_subsamp = floor(length(lon) / 30), arr_scale = 1, 
                        arr_ref_len = 15, arr_units = "m/s", 
                        arr_scale_shaft = 1, arr_scale_shaft_angle = 1,
                        axelab = TRUE, labW = FALSE, 
                        lab_dist_x = NULL, lab_dist_y = NULL, degree_sym = FALSE,
                        intylat = 20, intxlon = 20,
                        xlonshft = 0, ylatshft = 0, xlabels = NULL, ylabels = NULL,
                        axes_tick_scale = 1, axes_label_scale = 1,
                        drawleg = TRUE, subsampleg = NULL, 
                        bar_extra_labels = NULL, draw_bar_ticks = TRUE, 
                        draw_separators = FALSE, triangle_ends_scale = 1, 
                        bar_label_digits = 4, bar_label_scale = 1, 
                        units_scale = 1, bar_tick_scale = 1, 
                        bar_extra_margin = rep(0, 4),
                        boxlim = NULL, boxcol = 'purple2', boxlwd = 5, 
                        margin_scale = rep(1, 4), title_scale = 1, 
                        numbfig = NULL, fileout = NULL, 
                        width = 8, height = 5, size_units = 'in', 
                        res = 100, ...) {
  # Process the user graphical parameters that may be passed in the call
  ## Graphical parameters to exclude
  excludedArgs <- c("cex", "cex.axis", "cex.lab", "cex.main", "col", "din", "fig", "fin", "lab", "las", "lty", "lwd", "mai", "mar", "mgp", "new", "oma", "ps", "tck")
  userArgs <- .FilterUserGraphicArgs(excludedArgs, ...)

  # If there is any filenames to store the graphics, process them
  # to select the right device 
  if (!is.null(fileout)) {
    deviceInfo <- .SelectDevice(fileout = fileout, width = width, height = height, units = size_units, res = res)
    saveToFile <- deviceInfo$fun
    fileout <- deviceInfo$files
  }

  # Check lon, lat
  if (!is.numeric(lon) || !is.numeric(lat)) {
    stop("Parameters 'lon' and 'lat' must be numeric vectors.")
  }

  # Check var
  if (is.null(var)) {
    stop("Parameter 'var' cannot be NULL.")
  }
  if (!is.array(var)) {
    stop("Parameter 'var' must be a numeric array.")
  }

  transpose <- FALSE
  if (!is.null(names(dim(var)))) {
    if (any(names(dim(var)) %in% .KnownLonNames()) &&
        any(names(dim(var)) %in% .KnownLatNames())) {
      lon_dim <- names(dim(var))[names(dim(var)) %in% .KnownLonNames()]
      lat_dim <- names(dim(var))[names(dim(var)) %in% .KnownLatNames()]
    } else {
      names(dim(var)) <- NULL
      lat_dim <- NULL
      lon_dim <- NULL
      .warning("Dimension names of 'var' doesn't correspond to any coordinates names supported by s2dv package.")
    }
  } else {
    lon_dim <- NULL
    lat_dim <- NULL
    .warning("Parameter 'var' should have dimension names. Coordinates 'lon' and 'lat' have been assigned into the corresponding coordinates dimensions.")
  }

  if (length(dim(var)) > 2) {
    if (!is.null(lon_dim) & !is.null(lat_dim)) {
      dimnames <- names(dim(var))
      dim(var) <- dim(var)[which((dimnames == lon_dim | dimnames == lat_dim | dim(var) != 1))]
    } else {
      if (all(dim(var) == 1)) {
        dim(var) <- c(1, 1)
      } else if (length(dim(var)[which(dim(var) > 1)]) == 2) {
        var <- drop(var)
      } else if (length(dim(var)[which(dim(var) > 1)]) == 1) {
        dim(var) <- c(dim(var)[which(dim(var) > 1)], 1)
      }
    }
  }

  if (length(dim(var)) != 2) {
    stop("Parameter 'var' must be a numeric array with two dimensions.")
  }

  if ((dim(var)[1] == length(lon) && dim(var)[2] == length(lat)) ||
      (dim(var)[2] == length(lon) && dim(var)[1] == length(lat))) {
    if (dim(var)[2] == length(lon) && dim(var)[1] == length(lat)) {
      if (length(lon) == length(lat)) {
        if (is.null(names(dim(var)))) {
          .warning("Parameter 'var' should have dimension names. Coordinates 'lon' and 'lat' have been assigned into the first and second dimensions.")
        } else {
          if (names(dim(var)[1]) == lat_dim) {
            transpose <- TRUE
          }
        }
      } else {
        transpose <- TRUE
      }   
    }
  } else {
    stop("Parameters 'lon' and 'lat' must have as many elements as the number of cells along longitudes and latitudes in the input array 'var'.")
  }

  if (!is.null(names(dim(var)))) {
    if (names(dim(var)[1]) == lon_dim) {
      if (transpose) {
        stop("Coordinates dimensions of 'var' doesn't correspond to lat or lon.")
      }
    } else if (names(dim(var)[2]) == lon_dim) {
      if (!transpose) {
        stop("Coordinates dimensions of 'var' doesn't correspond to lat or lon.")
      }
    }
  }
  
  # Transpose the input matrices because the base plot functions work directly 
  # with dimensions c(lon, lat).

  if (transpose) {
    var <- t(var)
  }

  transpose <- FALSE

  names(dim(var)) <- c(lon_dim, lat_dim)
  dims <- dim(var)

  # Check varu and varv
  if (!is.null(varu) && !is.null(varv)) {
    if (!is.array(varu) || !(length(dim(varu)) == 2)) {
      stop("Parameter 'varu' must be a numerical array with two dimensions.")
    }
    if (!is.array(varv) || !(length(dim(varv)) == 2)) {
      stop("Parameter 'varv' must be a numerical array with two dimensions.")
    }
  } else if (!is.null(varu) || !is.null(varv)) {
    stop("Only one of the components 'varu' or 'varv' has been provided. Both must be provided.")
  }

  if (!is.null(varu) && !is.null(varv)) {
    if (!all(dim(varu) %in% dim(varv)) || !all(names(dim(varv)) %in% names(dim(varu)))) {
      stop("Parameter 'varu' and 'varv' must have equal dimensions and dimension names.")
    } else if (any(dim(varu) != dim(varv)) || any(names(dim(varv)) != names(dim(varu)))) {
      varv <- t(varv)
      names(dim(varv)) <- names(dim(varu))
    }

    if (is.null(lon_dim)) {
      names(dim(varu)) <- NULL
      names(dim(varv)) <- NULL
    } else {
      if (!is.null(names(dim(varu)))) {
        if (!(lon_dim %in% names(dim(varu)) && lat_dim %in% names(dim(varu)))) {
          stop("Parameters 'varu' and 'varv' must have same dimension names as 'var'.")
        } else if (dim(varu)[lon_dim] != dim(var)[lon_dim] || dim(varu)[lat_dim] != dim(var)[lat_dim]) {
          stop("Parameters 'varu' and 'varv' must have same dimensions as 'var'.")
        }
      } else {
        .warning("Parameters 'varu' and 'varv' should have dimension names. Coordinates 'lon' and 'lat' have been assigned into the corresponding coordinates dimensions.")
      }
    }
    

    if ((dim(varu)[1] == dims[1] && dim(varu)[2] == dims[2]) ||
        (dim(varu)[2] ==  dims[1] && dim(varu)[1] == dims[2])) {
      if (dim(varu)[2] == dims[1] && dim(varu)[1] == dims[2]) {
        if (length(lon) == length(lat)) {
          if (is.null(names(dim(varu)))) {
            .warning("Parameters 'varu' and 'varv' should have dimension names. Coordinates 'lon' and 'lat' have been assigned into the first and second dimensions.")
          } else {
            if (names(dim(varu)[1]) == lat_dim) {
              transpose <- TRUE
            }
          }
        } else {
          transpose <- TRUE
        }   
      } 
    } else {
      stop("Parameters 'lon' and 'lat' must have as many elements as the number of cells along longitudes and latitudes in the input array 'varu' and 'varv'.")
    }
    
    if (transpose) {
      varu <- t(varu)
      varv <- t(varv)
    }

    transpose <- FALSE

  }

  # Check contours
  if (!is.null(contours)) {
    if (!is.array(contours) || !(length(dim(contours)) == 2)) {
      stop("Parameter 'contours' must be a numerical array with two dimensions.")
    }
  }


  if (!is.null(contours)) {

    if (is.null(lon_dim)) {
      names(dim(contours)) <- NULL
    } else {
      if (!is.null(names(dim(contours)))) {
        if (!(lon_dim %in% names(dim(contours)) && lat_dim %in% names(dim(contours)))) {
          stop("Parameters 'contours' must have same dimension names as 'var'.")
        } else if (dim(contours)[lon_dim] != dim(var)[lon_dim] || dim(contours)[lat_dim] != dim(var)[lat_dim]) {
          stop("Parameters 'contours' must have same dimensions as 'var'.")
        }
      } else {
        .warning("Parameters 'contours' should have dimension names. Coordinates 'lon' and 'lat' have been assigned into the corresponding coordinates dimensions.")
      }
    }


    transpose <- FALSE
    if ((dim(contours)[1] == dims[1] && dim(contours)[2] == dims[2]) ||
        (dim(contours)[2] ==  dims[1] && dim(contours)[1] == dims[2])) {
      if (dim(contours)[2] == dims[1] && dim(contours)[1] == dims[2]) {
        if (length(lon) == length(lat)) {
          if (is.null(names(dim(contours)))) {
            .warning("Parameter 'contours' should have dimension names. Coordinates 'lon' and 'lat' have been assigned into the first and second dimensions.")
          } else {
            if (names(dim(contours)[1]) == lat_dim) {
              transpose <- TRUE
            }
          }
        } else {
          transpose <- TRUE
        }   
      }
    } else {
      stop("Parameters 'lon' and 'lat' must have as many elements as the number of cells along longitudes and latitudes in the input array 'contours'.")
    }

    if (transpose) {
      contours <- t(contours)
    }

    transpose <- FALSE

  }

  # Check toptitle
  if (is.null(toptitle) || is.na(toptitle)) {
    toptitle <- ''
  }
  if (!is.character(toptitle)) {
    stop("Parameter 'toptitle' must be a character string.")
  }

  # Check sizetit
  if (!is.null(sizetit)) {
    .warning("Parameter 'sizetit' is obsolete. Use 'title_scale' instead.")
    if (!is.numeric(sizetit) || length(sizetit) != 1) {
      stop("Parameter 'sizetit' must be a single numeric value.")
    }
    title_scale <- sizetit
  }

  if (!all(is.na(var))) {
    var_limits <- c(min(var[!is.infinite(var)], na.rm = TRUE),
                    max(var[!is.infinite(var)], na.rm = TRUE))
  } else {
    .warning("All the data are NAs. The map will be filled with colNA.")
    if (!is.null(brks) && length(brks) > 1) {
      #NOTE: var_limits be like this to avoid warnings from ColorBar
      var_limits <- c(min(brks, na.rm = TRUE) + diff(brks)[1], 
                      max(brks, na.rm = TRUE))
    } else if (!is.null(bar_limits)) {
      var_limits <- c(bar_limits[1] + 0.01, bar_limits[2])
    } else {
      var_limits <- c(-0.5, 0.5) # random range since colorbar is not going to be plotted
      if (drawleg) {
        drawleg <- FALSE
        .warning("All data are NAs. Color bar won't be drawn. If you want to have ",
                 "color bar still, define parameter 'brks' or 'bar_limits'.")
      }
    }
  }

  # Check: brks, cols, subsampleg, bar_limits, color_fun, bar_extra_labels, draw_bar_ticks
  #        draw_separators, triangle_ends_scale, label_scale, units, units_scale, 
  #        bar_label_digits
  # Build: brks, cols, bar_limits, col_inf, col_sup
  colorbar <- ColorBar(brks, cols, FALSE, subsampleg, bar_limits, var_limits,
                       triangle_ends, col_inf, col_sup, color_fun, FALSE, 
                       extra_labels = bar_extra_labels, draw_ticks = draw_bar_ticks,
                       draw_separators = draw_separators, 
                       triangle_ends_scale = triangle_ends_scale,
                       label_scale = bar_label_scale, title = units, 
                       title_scale = units_scale, tick_scale = bar_tick_scale,
                       extra_margin = bar_extra_margin, label_digits = bar_label_digits)
  brks <- colorbar$brks
  cols <- colorbar$cols
  col_inf <- colorbar$col_inf
  col_sup <- colorbar$col_sup
  bar_limits <- c(head(brks, 1), tail(brks, 1))

  # Check colNA
  if (is.null(colNA)) {
    if ('na_color' %in% names(attributes(cols))) {
      colNA <- attr(cols, 'na_color')
      if (!.IsColor(colNA)) {
        stop("The 'na_color' provided as attribute of the colour vector must be a valid colour identifier.")
      }
    } else {
      colNA <- 'pink'
    }
  } else if (!.IsColor(colNA)) {
    stop("Parameter 'colNA' must be a valid colour identifier.")
  }

  # Check square
  if (!is.logical(square)) {
    stop("Parameter 'square' must be logical.")
  }

  # Check filled.continents
  if (is.null(filled.continents)) {
    if (!square) {
      filled.continents <- FALSE
    } else {
      filled.continents <- TRUE
    }
  }
  if (!.IsColor(filled.continents) && !is.logical(filled.continents)) {
    stop("Parameter 'filled.continents' must be logical or a colour identifier.")
  } else if (!is.logical(filled.continents)) {
    continent_color <- filled.continents
    filled.continents <- TRUE
  } else {
    continent_color <- gray(0.5)
  }

  # Check filled.oceans
  if (!.IsColor(filled.oceans) & !is.logical(filled.oceans)) {
    stop("Parameter 'filled.oceans' must be logical or a colour identifier.")
  } else if (!is.logical(filled.oceans)) {
    ocean_color <- filled.oceans
    filled.oceans <- TRUE
  } else if (filled.oceans) {
    ocean_color <- "light blue"
  }

  # Check country.borders
  if (!is.logical(country.borders)) {
    stop("Parameter 'country.borders' must be logical.")
  }

  # Check coast_color
  if (is.null(coast_color)) {
    if (filled.continents) {
      coast_color <- continent_color
    } else {
      coast_color <- 'black'
    }
  }
  if (!.IsColor(coast_color)) {
    stop("Parameter 'coast_color' must be a valid colour identifier.")
  }

  # Check coast_width
  if (!is.numeric(coast_width)) {
    stop("Parameter 'coast_width' must be numeric.")
  }

  # Check lake_color
  if (!is.null(lake_color)) {
    if (!.IsColor(lake_color)) {
      stop("Parameter 'lake_color' must be a valid colour identifier.")
    }
  }

  # Check shapefile
  if (!is.null(shapefile)) {
    if (is.list(shapefile)) {
      shape <- shapefile
      if (any(!c('x', 'y') %in% names(shape))) {
        stop("The list names of the object in 'shapefile' .rds file should ", 
             "have at least 'x' and 'y'.")
      }
      if (length(shape$x) != length(shape$y)) {
        stop("The length of x and y in 'shapefile' list should be equal.")
      }
    } else if (!is.character(shapefile)) {
      stop("Parameter 'shapefile' must be a .rds file or a list.")
    } else {  # .rds file
      if (!file.exists(shapefile)) {
        stop("Parameter 'shapefile' is not a valid file.")
      }
      if (!grepl("\\.rds$", shapefile)) {
        stop("Parameter 'shapefile' must be a .rds file or a list.")
      }
      shape <- readRDS(file = shapefile)
      if (!is.list(shape)) {
        stop("Parameter 'shapefile' should be a .rds file of a list object.")
      }
      if (any(!c('x', 'y') %in% names(shape))) {
        stop("The list names of the object in 'shapefile' .rds file should ",
             "have at least 'x' and 'y'.")
      }
      if (length(shape$x) != length(shape$y)) {
        stop("The length of x and y in 'shapefile' list should be equal.")
      }
    }
  }

  # Check shapefile_col
  if (is.null(shapefile_color)) {
    if (filled.continents) {
      shapefile_color <- continent_color
    } else {
      shapefile_color <- 'black'
    }
  }
  if (!.IsColor(shapefile_color)) {
    stop("Parameter 'shapefile_color' must be a valid colour identifier.")
  }

  # Check brks2
  if (is.null(brks2)) {
    if (is.null(contours)) { 
      if (!square) {
        brks2 <- brks
        contours <- var 
      }
    } else {
      ll <- signif(min(contours, na.rm = TRUE), 2)
      ul <- signif(max(contours, na.rm = TRUE), 2)
      brks2 <- signif(seq(ll, ul, length.out = length(brks)), 2)
    }
  }

  # Check contour_lwd
  if (!is.numeric(contour_lwd)) {
    stop("Parameter 'contour_lwd' must be numeric.")
  }

  # Check contour_color
  if (!.IsColor(contour_color)) {
    stop("Parameter 'contour_color' must be a valid colour identifier.")
  }

  # Check contour_lty
  if (!is.numeric(contour_lty) && !is.character(contour_lty)) {
    stop("Parameter 'contour_lty' must be either a number or a character string.")
  }

  # Check contour_draw_label
  if (!is.logical(contour_draw_label)) {
    stop("Parameter 'contour_draw_label' must be logical.")
  }

  # Check contour_label_scale
  if (!is.numeric(contour_label_scale)) {
    stop("Parameter 'contour_label_scale' must be numeric.")
  }

  # Check dots
  if (!is.null(dots)) {
    if (!is.array(dots) || !(length(dim(dots)) %in% c(2, 3))) {
      stop("Parameter 'dots' must be a logical array with two or three dimensions.")
    }
    if (length(dim(dots)) == 2) {
      dim(dots) <- c(1, dim(dots))
    }

    if (is.null(lon_dim)) {
      names(dim(dots)) <- NULL
    } else {
      if (!is.null(names(dim(dots)))) {
        if (!(lon_dim %in% names(dim(dots)) && lat_dim %in% names(dim(dots)))) {
          stop("Parameters 'dots' must have same dimension names as 'var'.")
        } else if (dim(dots)[lon_dim] != dim(var)[lon_dim] || dim(dots)[lat_dim] != dim(var)[lat_dim]) {
          stop("Parameters 'dots' must have same dimensions as 'var'.")
        }
      } else {
        .warning("Parameters 'dots' should have dimension names. Coordinates 'lon' and 'lat' have been assigned into the corresponding coordinates dimensions.")
      }
    }

    transpose <- FALSE
    if ((dim(dots)[2] == dims[1] && dim(dots)[3] == dims[2]) ||
        (dim(dots)[3] ==  dims[1] && dim(dots)[2] == dims[2])) {
      if (dim(dots)[3] == dims[1] && dim(dots)[2] == dims[2]) {
        if (length(lon) == length(lat)) {
          if (is.null(names(dim(dots)))) {
            .warning("Parameter 'dots' should have dimension names. Coordinates 'lon' and 'lat' have been assigned into the first and second dimensions.")
          } else {
            if (names(dim(dots)[2]) == lat_dim) {
              transpose <- TRUE
            }
          }
        } else {
          transpose <- TRUE
        }
      }
    } else {
      stop("Parameter 'dots' must have same number of longitudes and latitudes as 'var'.")
    }

    if (transpose) {
      dots <- aperm(dots, c(1, 3, 2))
    }

    transpose <- FALSE

  }

  # Check dot_symbol and dot_size
  if (!is.null(dots)) {
    if (!is.numeric(dot_symbol) && !is.character(dot_symbol)) {
      stop("Parameter 'dot_symbol' must be a numeric or character string vector.")
    }
    if (length(dot_symbol) == 1) {
      dot_symbol <- rep(dot_symbol, dim(dots)[1])
    } else if (length(dot_symbol) < dim(dots)[1]) {
      stop("Parameter 'dot_symbol' does not contain enough symbols.")
    }
    if (!is.numeric(dot_size)) {
      stop("Parameter 'dot_size' must be numeric.")
    }
    if (length(dot_size) == 1) {
      dot_size <- rep(dot_size, dim(dots)[1])
    } else if (length(dot_size) < dim(dots)[1]) {
      stop("Parameter 'dot_size' does not contain enough sizes.")
    }
  }

  # Check arrow parameters
  if (!is.numeric(arr_subsamp)) {
    stop("Parameter 'arr_subsamp' must be numeric.")
  }
  if (!is.numeric(arr_scale)) {
    stop("Parameter 'arr_scale' must be numeric.")
  }
  if (!is.numeric(arr_ref_len)) {
    stop("Parameter 'arr_ref_len' must be numeric.")
  }
  if (!is.character(arr_units)) {
    stop("Parameter 'arr_units' must be character.")
  }
  if (!is.numeric(arr_scale_shaft)) {
    stop("Parameter 'arr_scale_shaft' must be numeric.")
  }
  if (!is.numeric(arr_scale_shaft_angle)) {
    stop("Parameter 'arr_scale_shaft_angle' must be numeric.")
  }

  # Check axis parameters
  if (!is.logical(axelab)) {
    stop("Parameter 'axelab' must be logical.")
  }
  if (!is.logical(labW)) {
    stop("Parameter 'labW' must be logical.")
  }
  if (!is.null(lab_dist_x)) {
    if (!is.numeric(lab_dist_x)) {
      stop("Parameter 'lab_dist_x' must be numeric.")
    }
  }
  if (!is.null(lab_dist_y)) {
    if (!is.numeric(lab_dist_y)) {
      stop("Parameter 'lab_dist_y' must be numeric.")
    }
  }
  if (!is.numeric(intylat)) {
    stop("Parameter 'intylat' must be numeric.")
  } else {
    intylat <- round(intylat)
  }
  if (!is.numeric(intxlon)) {
    stop("Parameter 'intxlon' must be numeric.")
  } else {
    intxlon <- round(intxlon)
  }
  if (!is.numeric(xlonshft) | length(xlonshft) != 1) {
    stop("Parameter 'xlonshft' must be a number.")
  }
  if (!is.numeric(ylatshft) | length(ylatshft) != 1) {
    stop("Parameter 'ylatshft' must be a number.")
  }
  if (!is.null(xlabels)) {
    if (!is.character(xlabels) | !is.vector(xlabels)) {
      stop("Parameter 'xlabels' must be a vector of character string.")
    }
  }
  if (!is.null(ylabels)) {
    if (!is.character(ylabels) | !is.vector(ylabels)) {
      stop("Parameter 'ylabels' must be a vector of character string.")
    }
  }

  # Check legend parameters
  if (!is.logical(drawleg)) {
    stop("Parameter 'drawleg' must be logical.")
  }

  # Check box parameters
  if (!is.null(boxlim)) {
    if (!is.list(boxlim)) {
      boxlim <- list(boxlim)
    }
    for (i in 1:length(boxlim)) {
      if (!is.numeric(boxlim[[i]]) || length(boxlim[[i]]) != 4) {
        stop("Parameter 'boxlim' must be a a numeric vector or a list of numeric vectors of length 4 (with W, S, E, N box limits).")
      }
    }
    if (!is.character(boxcol)) {
      stop("Parameter 'boxcol' must be a character string or a vector of character strings.")
    } else {
      if (length(boxlim) != length(boxcol)) {
        if (length(boxcol) == 1) {
          boxcol <- rep(boxcol, length(boxlim))
        } else {
          stop("Parameter 'boxcol' must have a colour for each box in 'boxlim' or a single colour for all boxes.")
        }
      }
    }
    if (!is.numeric(boxlwd)) {
      stop("Parameter 'boxlwd' must be numeric.")
    } else {
      if (length(boxlim) != length(boxlwd)) {
        if (length(boxlwd) == 1) {
          boxlwd <- rep(boxlwd, length(boxlim))
        } else {
          stop("Parameter 'boxlwd' must have a line width for each box in 'boxlim' or a single line width for all boxes.")
        }
      }
    }
  }

  # Check margin_scale
  if (!is.numeric(margin_scale) || length(margin_scale) != 4) {
    stop("Parameter 'margin_scale' must be a numeric vector of length 4.")
  }

  # Check title_scale
  if (!is.numeric(title_scale)) {
    stop("Parameter 'title_scale' must be numeric.")
  }

  # Check axes_tick_scale
  if (!is.numeric(axes_tick_scale)) {
    stop("Parameter 'axes_tick_scale' must be numeric.")
  }

  # Check axes_label_scale
  if (!is.numeric(axes_label_scale)) {
    stop("Parameter 'axes_label_scale' must be numeric.")
  }

  # Check numbfig
  if (!is.null(numbfig)) {
    if (!is.numeric(numbfig)) {
      stop("Parameter 'numbfig' must be numeric.")
    } else {
      numbfig <- round(numbfig)
      scale <- 1 / numbfig ** 0.3
      axes_tick_scale <- axes_tick_scale * scale
      axes_label_scale <- axes_label_scale * scale
      title_scale <- title_scale * scale
      margin_scale <- margin_scale * scale
      arr_scale <- arr_scale * scale
      dot_size <- dot_size * scale
      contour_label_scale <- contour_label_scale * scale
      contour_lwd <- contour_lwd * scale
    }
  }

  #
  #  Input arguments 
  # ~~~~~~~~~~~~~~~~~
  #
  latb <- sort(lat, index.return = TRUE)
  dlon <- diff(lon)
  wher <- which(dlon > (mean(dlon) + 1))
  if (length(wher) > 0) {
    .warning("Detect gap in 'lon' vector, which is considered as crossing the border.")
    lon[(wher + 1):dims[1]] <- lon[(wher + 1):dims[1]] - 360
  }
  lonb <- sort(lon, index.return = TRUE)
  latmin <- floor(min(lat) / 10) * 10
  latmax <- ceiling(max(lat) / 10) * 10
  lonmin <- floor(min(lon) / 10) * 10
  lonmax <- ceiling(max(lon) / 10) * 10

  #
  #  Plotting the map
  # ~~~~~~~~~~~~~~~~~~
  #

  # Open connection to graphical device
  if (!is.null(fileout)) {
    saveToFile(fileout)
  } else if (names(dev.cur()) == 'null device') {
    dev.new(units = size_units, res = res, width = width, height = height)
  }

  #
  #  Defining the layout
  # ~~~~~~~~~~~~~~~~~~~~~
  # 
  if (drawleg) {
    margin_scale[1] <- margin_scale[1] - 1
  }
  margins <- rep(0.4, 4) * margin_scale
  margins[4] <- margins[4] + 1
  cex_title <- 2 * title_scale
  cex_axes_labels <- 1.3 * axes_label_scale
  cex_axes_ticks <- -0.5 * axes_tick_scale
  spaceticklab <- 0
  if (axelab) { 
    # Y axis label
    if (!is.null(ylabels)) {
      ypos <- seq(latmin, latmax, intylat) + ylatshft
      if (length(ypos) != length(ylabels)) {
        stop(paste0("Parameter 'ylabels' must have the same length as the latitude ",
                    "vector spaced by 'intylat' (length = ", length(ypos), ")."))
      }
      ylabs <- ylabels
    } else {
      ypos <- seq(latmin, latmax, intylat) + ylatshft
      letters <- array('', length(ypos))
      if (degree_sym == FALSE) {
        letters[ypos < 0] <- 'S'
        letters[ypos > 0] <- 'N'
      } else {
        letters[ypos < 0] <- paste(intToUtf8(176), 'S')
        letters[ypos > 0] <- paste(intToUtf8(176), 'N')
      }
      ylabs <- paste(as.character(abs(ypos)), letters, sep = '')
    }

    # X axis label
    if (!is.null(xlabels)) {
      xpos <- seq(lonmin, lonmax, intxlon) + xlonshft
      if (length(xpos) != length(xlabels)) {
        stop(paste0("Parameter 'xlabels' must have the same length as the longitude ",
                    "vector spaced by 'intxlon' (length = ", length(xpos), ")."))
      }
      xlabs <- xlabels
    } else {
      xpos <- seq(lonmin, lonmax, intxlon) + xlonshft
      letters <- array('', length(xpos))
      if (labW) {
        xpos2 <- xpos  
        xpos2[xpos2 > 180] <- 360 - xpos2[xpos2 > 180]
      }
      if (degree_sym == FALSE) {
        letters[xpos < 0] <- 'W'
        letters[xpos > 0] <- 'E'
      } else {
        letters[xpos < 0] <- paste(intToUtf8(176), 'W')
        letters[xpos > 0] <- paste(intToUtf8(176), 'E')
      }
      if (labW) {
        letters[xpos == 0] <- ' '
        letters[xpos == 180] <- ' '
        if (degree_sym == FALSE) {
          letters[xpos > 180] <- 'W'
        } else {
          letters[xpos > 180] <- paste(intToUtf8(176), 'W')
        }  
        xlabs <- paste(as.character(abs(xpos2)), letters, sep = '')
      } else {
        xlabs <- paste(as.character(abs(xpos)), letters, sep = '')
      }
    }
    spaceticklab <- max(-cex_axes_ticks, 0)
    margins[1] <- margins[1] + 1.2 * cex_axes_labels + spaceticklab
    margins[2] <- margins[2] + 1.2 * cex_axes_labels + spaceticklab
  }
  bar_extra_margin[2] <- bar_extra_margin[2] + margins[2]
  bar_extra_margin[4] <- bar_extra_margin[4] + margins[4]
  if (toptitle != '') {
    margins[3] <- margins[3] + cex_title + 1
  }
  if (!is.null(varu)) {
    margins[1] <- margins[1] + 2.2 * units_scale
  }

  if (drawleg) {
    layout(matrix(1:2, ncol = 1, nrow = 2), heights = c(5, 1))
  }
  plot.new()
  # Load the user parameters
  par(userArgs)
  par(mar = margins, cex.main = cex_title, cex.axis = cex_axes_labels,
      mgp = c(0, spaceticklab, 0), las = 0)

  #NOTE: Here creates the window for later plot. If 'usr' for par() is not specified,
  #      use the lat/lon as the borders. If 'usr' is specified, use the assigned values.
  if (is.null(userArgs$usr)) { 
    #NOTE: The grids are assumed to be equally spaced
    xlim_cal <- c(lonb$x[1] - (lonb$x[2] - lonb$x[1]) / 2, 
                  lonb$x[length(lonb$x)] + (lonb$x[2] - lonb$x[1]) / 2)
    ylim_cal <- c(latb$x[1] - (latb$x[2] - latb$x[1]) / 2,
                  latb$x[length(latb$x)] + (latb$x[2] - latb$x[1]) / 2)
    plot.window(xlim =  xlim_cal, ylim = ylim_cal, xaxs = 'i', yaxs = 'i')
# Below is Old code. The border grids are only half plotted.
#    plot.window(xlim = range(lonb$x, finite = TRUE), ylim = range(latb$x, finite = TRUE),
#                xaxs = 'i', yaxs = 'i')
  } else {
    plot.window(xlim = par("usr")[1:2], ylim  = par("usr")[3:4], xaxs = 'i', yaxs = 'i')
  }

  if (axelab) {
    lab_distance_y <- ifelse(is.null(lab_dist_y), spaceticklab + 0.2, lab_dist_y)
    lab_distance_x <- ifelse(is.null(lab_dist_x), spaceticklab + cex_axes_labels / 2 - 0.3, lab_dist_x)

    axis(2, at = ypos, labels = ylabs, cex.axis = cex_axes_labels, tcl = cex_axes_ticks,
         mgp = c(0, lab_distance_y, 0))
    axis(1, at = xpos, labels = xlabs, cex.axis = cex_axes_labels, tcl = cex_axes_ticks,
         mgp = c(0, lab_distance_x, 0))
  }
  title(toptitle, cex.main = cex_title)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = colNA)
  col_inf_image <- ifelse(is.null(col_inf), colNA, col_inf)
  col_sup_image <- ifelse(is.null(col_sup), colNA, col_sup)
  if (square) {
    # If lat and lon are both regular-spaced, "useRaster = TRUE" can avoid
    # artifact white lines on the figure. If not, useRaster has to be FALSE (default)
    tryCatch({
      image(lonb$x, latb$x, var[lonb$ix, latb$ix], 
            col = c(col_inf_image, cols, col_sup_image), 
            breaks = c(-.Machine$double.xmax, brks, .Machine$double.xmax),
            axes = FALSE, xlab = "", ylab = "", add = TRUE, useRaster = TRUE)
    }, error = function(x) {
      image(lonb$x, latb$x, var[lonb$ix, latb$ix], 
            col = c(col_inf_image, cols, col_sup_image), 
            breaks = c(-.Machine$double.xmax, brks, .Machine$double.xmax),
            axes = FALSE, xlab = "", ylab = "", add = TRUE)
    })
  } else {
    .filled.contour(lonb$x, latb$x, var[lonb$ix, latb$ix], 
                    levels = c(.Machine$double.xmin, brks, .Machine$double.xmax), 
                    col = c(col_inf_image, cols, col_sup_image))
  }
  if (!is.null(contours)) {
#NOTE: 'labcex' is the absolute size of contour labels. Parameter 'contour_label_scale'
#      is provided in PlotEquiMap() but it was not used. Here, 'cex_axes_labels' was used
#      and it was calculated from 'axes_label_scale', the size of lat/lon axis label.
#      It is changed to use contour_label_scale*par('cex').
    contour(lonb$x, latb$x, contours[lonb$ix, latb$ix], levels = brks2,
            method = "edge", add = TRUE,
#            labcex = cex_axes_labels,
            labcex = contour_label_scale * par("cex"),
            lwd = contour_lwd, lty = contour_lty,
            col = contour_color, drawlabels = contour_draw_label)
  }

  #
  #  Adding black dots or symbols
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  if (!is.null(dots)) {
    data_avail <- !is.na(var)
    for (counter in 1:(dim(dots)[1])) {
      points <- which(dots[counter, , ] & data_avail, arr.ind = TRUE)
      points(lon[points[, 1]], lat[points[, 2]], 
             pch = dot_symbol[counter], 
             cex = dot_size[counter] * 3 / sqrt(sqrt(length(var))),
             lwd = dot_size[counter] * 3 / sqrt(sqrt(length(var))))
    }
  }
  #
  #  Plotting continents
  # ~~~~~~~~~~~~~~~~~~~~~
  #
  wrap_vec <- c(lonb$x[1], lonb$x[1] + 360)
  old_lwd <- par('lwd')
  par(lwd = coast_width)
  # If [0, 360], use GEOmap; if [-180, 180], use maps::map
  # UPDATE: Use maps::map for both cases. The difference between GEOmap and
  #         maps is trivial. The only thing we can see for now is that 
  #         GEOmap has better lakes.
  coast <- maps::map(interior = country.borders, wrap = wrap_vec,
                     fill = filled.continents, add = TRUE, plot = FALSE)

  if (filled.continents) {
    polygon(coast, col = continent_color, border = coast_color, lwd = coast_width)
  } else {
    lines(coast, col = coast_color, lwd = coast_width)
  }
  if (!is.null(lake_color)) {
    maps::map('lakes', add = TRUE, wrap = wrap_vec, fill = filled.continents, col = lake_color) 
  }
  par(lwd = old_lwd)

  # filled.oceans
  if (filled.oceans) {
      old_lwd <- par('lwd')
      par(lwd = coast_width)

      outline <- maps::map(wrap = wrap_vec, fill = T, plot = FALSE)  # must be fill = T
      xbox <- wrap_vec + c(-2, 2)
      ybox <- c(-92, 92) 
      outline$x <- c(outline$x, NA, c(xbox, rev(xbox), xbox[1]))
      outline$y <- c(outline$y, NA, rep(ybox, each = 2), ybox[1])
      polypath(outline, col = ocean_color, rule = 'evenodd', border = NA)

      par(lwd = old_lwd)
  }

  # Plot shapefile
  #NOTE: the longitude range cannot cut shapefile range, or not all the shapefile will be plotted.
  if (!is.null(shapefile)) {
    maps::map(shape, interior = country.borders, #wrap = wrap_vec,
              fill = filled.continents, add = TRUE, plot = TRUE, 
              lwd = shapefile_lwd, col = shapefile_color)
  }

  box()
  # Draw rectangle on the map
  if (!is.null(boxlim)) {
    counter <- 1
    for (box in boxlim) {
      if (box[1] > box[3]) {
        box[1] <- box[1] - 360
      }
      if (length(box) != 4) {
        stop(paste("The", counter, "st box defined in the parameter 'boxlim' is ill defined."))
      } else if (box[2] < latmin || box[4] > latmax || 
                 box[1] < lonmin || box[3] > lonmax) {
        stop(paste("The limits of the", counter, "st box defined in the parameter 'boxlim' are invalid."))
      } else if (box[1] < 0 && box[3] > 0) {
        #segments south
        segments(box[1], box[2], 0, box[2], col = boxcol[counter], lwd = boxlwd[counter])
        segments(0, box[2], box[3], box[2], col = boxcol[counter], lwd = boxlwd[counter]) 
        #segments north
        segments(box[1], box[4], 0, box[4], col = boxcol[counter], lwd = boxlwd[counter])
        segments(0, box[4], box[3], box[4], col = boxcol[counter], lwd = boxlwd[counter]) 
        #segments west
        segments(box[1], box[2], box[1], box[4], col = boxcol[counter], 
                 lwd = boxlwd[counter])  
        #segments est
        segments(box[3], box[2], box[3],box[4], col = boxcol[counter], 
                 lwd = boxlwd[counter])          
      } else {
        rect(box[1], box[2], box[3], box[4], border = boxcol[counter], col = NULL, 
             lwd = boxlwd[counter], lty = 'solid')
      }
      counter <- counter + 1
    }
  }
  #
  #  PlotWind
  # ~~~~~~~~~~
  #
  if (!is.null(varu) && !is.null(varv)) {
    # Create a two dimention array of longitude and latitude
    lontab <- InsertDim(lonb$x, 2, length(latb$x), name = 'lat')
    lattab <- InsertDim(latb$x, 1, length(lonb$x), name = 'lon')
    varplotu <- varu[lonb$ix, latb$ix]
    varplotv <- varv[lonb$ix, latb$ix]

    # Select a subsample af the points to an arrow
    #for each "subsample" grid point
    sublon <- seq(1,length(lon), arr_subsamp)
    sublat <- seq(1,length(lat), arr_subsamp)

    uaux <- lontab[sublon, sublat] + varplotu[sublon, sublat] * 0.5 * arr_scale
    vaux <- lattab[sublon, sublat] + varplotv[sublon, sublat] * 0.5 * arr_scale

    lenshaft <- 0.18 * arr_scale * arr_scale_shaft
    angleshaft <- 12 * arr_scale_shaft_angle
    # Plot Wind
    arrows(lontab[sublon, sublat], lattab[sublon, sublat],
           uaux, vaux,
           angle = angleshaft,
           length = lenshaft)
    
    # Plotting an arrow at the bottom of the plot for the legend
    posarlon <- lonb$x[1] + (lonmax - lonmin) * 0.1
    posarlat <- latmin - ((latmax - latmin) + 1) / par('pin')[2] * 
                         (spaceticklab + 0.2 + cex_axes_labels + 0.6 * units_scale) * par('csi')

    arrows(posarlon, posarlat,
           posarlon + 0.5 * arr_scale * arr_ref_len, posarlat,
           length = lenshaft, angle = angleshaft,
           xpd = TRUE)
    #save the parameter value
    xpdsave <- par('xpd')
    #desactivate xpd to be able to plot in margen
    par(xpd = NA)
    #plot text
    mtext(paste(as.character(arr_ref_len), arr_units, sep = ""),
          line = spaceticklab + 0.2 + cex_axes_labels + 1.2 * units_scale, side = 1,
          at = posarlon + (0.5 * arr_scale * arr_ref_len) / 2,
          cex = units_scale)
    #come back to the previous xpd value
    par(xpd = xpdsave)
  }
  #
  #  Colorbar
  # ~~~~~~~~~~
  #
  if (drawleg) {
    ColorBar(brks, cols, FALSE, subsampleg, bar_limits, var_limits, 
             triangle_ends, col_inf = col_inf, col_sup = col_sup, 
             extra_labels = bar_extra_labels, draw_ticks = draw_bar_ticks,
             draw_separators = draw_separators, title = units, 
             title_scale = units_scale, triangle_ends_scale = triangle_ends_scale, 
             label_scale = bar_label_scale, tick_scale = bar_tick_scale,
             extra_margin = bar_extra_margin, label_digits = bar_label_digits)
  }

  # If the graphic was saved to file, close the connection with the device
  if (!is.null(fileout)) dev.off()

  invisible(list(brks = brks, cols = cols, col_inf = col_inf, col_sup = col_sup))
}
