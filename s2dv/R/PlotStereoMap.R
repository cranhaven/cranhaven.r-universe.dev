#'Maps A Two-Dimensional Variable On A Polar Stereographic Projection
#'
#'Map longitude-latitude array (on a regular rectangular or gaussian grid) on 
#'a polar stereographic world projection with coloured grid cells. Only the 
#'region within a specified latitude interval is displayed. A colour bar 
#'(legend) can be plotted and adjusted. It is possible to draw superimposed 
#'dots, symbols, boxes, contours, and arrows. A number of options is provided to
#'adjust the position, size and colour of the components. This plot function is 
#'compatible with figure layouts if colour bar is disabled.
#'
#'@param var Array with the values at each cell of a grid on a regular 
#'  rectangular or gaussian grid. The array is expected to have two dimensions: 
#'  c(latitude, longitude). Longitudes can be in ascending or descending order 
#'  and latitudes in any order. It can contain NA values (coloured with 
#'  'colNA'). Arrays with dimensions c(longitude, latitude) will also be 
#'  accepted but 'lon' and 'lat' will be used to disambiguate so this 
#'  alternative is not appropriate for square arrays.
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
#'  the same dimensions as 'var'.
#'@param varv Array of the meridional component of wind/current/other field 
#'  with the same dimensions as 'var'.
#'@param latlims Latitudinal limits of the figure.\cr
#'  Example : c(60, 90) for the North Pole\cr
#'            c(-90,-60) for the South Pole
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
#'@param color_fun,subsampleg,bar_extra_labels,draw_bar_ticks,draw_separators,triangle_ends_scale,bar_label_digits,bar_label_scale,units_scale,bar_tick_scale,bar_extra_margin Set of parameters to control the visual 
#'  aspect of the drawn colour bar. See ?ColorBar for a full explanation.
#'@param filled.continents Colour to fill in drawn projected continents. Takes 
#'  the value gray(0.5) by default. If set to FALSE, continents are not 
#'  filled in.
#'@param coast_color Colour of the coast line of the drawn projected 
#'  continents. Takes the value gray(0.5) by default.
#'@param coast_width Line width of the coast line of the drawn projected 
#'  continents. Takes the value 1 by default.
#'@param contours Array of same dimensions as 'var' to be added to the plot 
#'  and displayed with contours. Parameter 'brks2' is required to define the 
#'  magnitude breaks for each contour curve.
#'@param brks2 A numeric value or vector of magnitude breaks where to draw 
#'  contour curves for the array provided in 'contours'. If it is a number, it
#'  represents the number of breaks (n) that defines (n - 1) intervals to 
#'  classify 'contours'.
#'@param contour_lwd Line width of the contour curves provided via 'contours' 
#'  and 'brks2'. The default value is 0.5.
#'@param contour_color Line color of the contour curves provided via 'contours' 
#'  and 'brks2'.
#'@param contour_lty Line type of the contour curves. Takes 1 (solid) by 
#'  default. See help on 'lty' in par() for other accepted values.
#'@param contour_label_draw A logical value indicating whether to draw the 
#'  contour labels (TRUE) or not (FALSE) when 'contours' is used. The default
#'  value is TRUE.
#'@param contour_label_scale Scale factor for the superimposed labels when 
#'  drawing contour levels. The default value is 0.6.
#'@param dots Array of same dimensions as 'var' or with dimensions 
#'  c(n, dim(var)), where n is the number of dot/symbol layers to add to the 
#'  plot. A value of TRUE at a grid cell will draw a dot/symbol on the 
#'  corresponding square of the plot. By default all layers provided in 'dots' 
#'  are plotted with dots, but a symbol can be specified for each of the 
#'  layers via the parameter 'dot_symbol'.
#'@param dot_symbol Single character/number or vector of characters/numbers 
#'  that correspond to each of the symbol layers specified in parameter 'dots'. 
#'  If a single value is specified, it will be applied to all the layers in 
#'  'dots'. Takes 15 (centered square) by default. See 'pch' in par() for 
#'  additional accepted options.
#'@param dot_size Scale factor for the dots/symbols to be plotted, specified 
#'  in 'dots'. If a single value is specified, it will be applied to all 
#'  layers in 'dots'. Takes 1 by default.
#'@param intlat Interval between latitude lines (circles), in degrees. 
#'  Defaults to 10.
#'@param arr_subsamp A number as subsampling factor to select a subset of arrows 
#'  in 'varu' and 'varv' to be drawn. Only one out of arr_subsamp arrows will 
#'  be drawn. The default value is 1.
#'@param arr_scale A number as scale factor for drawn arrows from 'varu' and 
#'  'varv'. The default value is 1.
#'@param arr_ref_len A number of the length of the refence arrow to be drawn as
#'  legend at the bottom of the figure (in same units as 'varu' and 'varv', only
#'  affects the legend for the wind or variable in these arrays). The default 
#'  value is 15.
#'@param arr_units Units of 'varu' and 'varv', to be drawn in the legend. 
#'  Takes 'm/s' by default.
#'@param arr_scale_shaft A number for the scale of the shaft of the arrows 
#'  (which also depend on the number of figures and the arr_scale parameter). 
#'  The default value is 1.
#'@param arr_scale_shaft_angle A number for the scale of the angle of the 
#'  shaft of the arrows (which also depend on the number of figure and the 
#'  arr_scale parameter). The default value is 1.
#'@param drawleg Whether to plot a color bar (legend, key) or not. 
#'  Defaults to TRUE.
#'@param boxlim Limits of a box to be added to the plot, in degrees: 
#'  c(x1, y1, x2, y2). A list with multiple box specifications can also 
#'  be provided.
#'@param boxcol Colour of the box lines. A vector with a colour for each of 
#'  the boxes is also accepted. Defaults to 'purple2'.
#'@param boxlwd Line width of the box lines. A vector with a line width for 
#'  each of the boxes is also accepted. Defaults to 5.
#'@param margin_scale Scale factor for the margins to be added to the plot, 
#'  with the format c(y1, x1, y2, x2). Defaults to rep(1, 4). If drawleg = TRUE, 
#'  margin_scale[1] is subtracted 1 unit.
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
#'@param \dots Arguments to be passed to the method. Only accepts the 
#'  following graphical parameters:\cr
#'  adj ann ask bg bty cex.sub cin col.axis col.lab col.main col.sub cra crt 
#'  csi cxy err family fg font font.axis font.lab font.main font.sub lend 
#'  lheight ljoin lmitre mex mfcol mfrow mfg mkh omd omi page pch pin plt pty 
#'  smo srt tcl usr xaxp xaxs xaxt xlog xpd yaxp yaxs yaxt ylbias ylog \cr
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
#'    drawn at all).
#'}
#'\item{col_sup}{
#'  Colour used to draw the upper triangle end in the colour bar (NULL if not 
#'    drawn at all).
#'}
#'
#'@examples
#'data <- matrix(rnorm(100 * 50), 100, 50)
#'x <- seq(from = 0, to = 360, length.out = 100)
#'y <- seq(from = -90, to = 90, length.out = 50)
#'  \dontrun{
#'PlotStereoMap(data, x, y, latlims = c(60, 90), brks = 50,
#'              toptitle = "This is the title")
#' }
#'@import mapproj
#'@importFrom grDevices dev.cur dev.new dev.off gray
#'@importFrom stats median
#'@export
PlotStereoMap <- function(var, lon, lat, varu = NULL, varv = NULL, latlims = c(60, 90), 
                          toptitle = NULL, sizetit = NULL, units = NULL, 
                          brks = NULL, cols = NULL, bar_limits = NULL, 
                          triangle_ends = NULL, col_inf = NULL, col_sup = NULL,
                          colNA = NULL, color_fun = clim.palette(),
                          filled.continents = FALSE, coast_color = NULL, 
                          coast_width = 1,
                          contours = NULL, brks2 = NULL, contour_lwd = 0.5,
                          contour_color = 'black', contour_lty = 1,
                          contour_label_draw = TRUE, contour_label_scale = 0.6,
                          dots = NULL, dot_symbol = 4, dot_size = 0.8,
                          intlat = 10,
                          arr_subsamp = floor(length(lon) / 30), arr_scale = 1,
                          arr_ref_len = 15, arr_units = "m/s",
                          arr_scale_shaft = 1, arr_scale_shaft_angle = 1, 
                          drawleg = TRUE, subsampleg = NULL, 
                          bar_extra_labels = NULL, draw_bar_ticks = TRUE, 
                          draw_separators = FALSE, triangle_ends_scale = 1,
                          bar_label_digits = 4, bar_label_scale = 1, 
                          units_scale = 1, bar_tick_scale = 1,
                          bar_extra_margin = rep(0, 4), 
                          boxlim = NULL, boxcol = "purple2", boxlwd = 5, 
                          margin_scale = rep(1, 4), title_scale = 1,
                          numbfig = NULL, fileout = NULL, 
                          width = 6, height = 5, size_units = 'in', 
                          res = 100, ...) {
  # Process the user graphical parameters that may be passed in the call
  ## Graphical parameters to exclude
  excludedArgs <- c("cex", "cex.main", "col", "fin", "lab", "las", "lwd", "mai", "mar", "mgp", "new", "pch", "ps")
  userArgs <- .FilterUserGraphicArgs(excludedArgs, ...)

  # If there is any filenames to store the graphics, process them
  # to select the right device 
  if (!is.null(fileout)) {
    deviceInfo <- .SelectDevice(fileout = fileout, width = width, height = height, units = size_units, res = res)
    saveToFile <- deviceInfo$fun
    fileout <- deviceInfo$files
  }

  # Preliminar check of dots, lon, lat
  if (!is.null(dots)) {
    if (!is.array(dots) || !(length(dim(dots)) %in% c(2, 3))) {
      stop("Parameter 'dots' must be a logical array with two or three dimensions.")
    }
    if (length(dim(dots)) == 2) {
      dim(dots) <- c(1, dim(dots))
    }
  }
  if (!is.numeric(lon) || !is.numeric(lat)) {
    stop("Parameters 'lon' and 'lat' must be numeric vectors.")
  }

  # Check var
  if (!is.array(var)) {
    stop("Parameter 'var' must be a numeric array.")
  }
  if (length(dim(var)) > 2) {
    var <- drop(var)
    dim(var) <- head(c(dim(var), 1, 1), 2)
  }
  if (length(dim(var)) > 2) {
    stop("Parameter 'var' must be a numeric array with two dimensions. See PlotMultiMap() for multi-pannel maps or AnimateMap() for animated maps.")
  } else if (length(dim(var)) < 2) {
    stop("Parameter 'var' must be a numeric array with two dimensions.")
  }
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
    if (dim(varu)[1] != dims[1] || dim(varu)[2] != dims[2]) {
      stop("Parameter 'varu' must have same number of longitudes and latitudes as 'var'.")
    }
    if (dim(varv)[1] != dims[1] || dim(varv)[2] != dims[2]) {
      stop("Parameter 'varv' must have same number of longitudes and latitudes as 'var'.")
    }
  }

  # Transpose the input matrices because the base plot functions work directly 
  # with dimensions c(lon, lat).
  if (dims[1] != length(lon) || dims[2] != length(lat)) {
    if (dims[1] == length(lat) && dims[2] == length(lon)) {
      var <- t(var)
      if (!is.null(varu)) varu <- t(varu)
      if (!is.null(varv)) varv <- t(varv)
      if (!is.null(dots)) dots <- aperm(dots, c(1, 3, 2))
      dims <- dim(var)
    }
  }

  # Check lon
  if (length(lon) != dims[1]) {
    stop("Parameter 'lon' must have as many elements as the number of cells along longitudes in the input array 'var'.")
  }

  # Check lat
  if (length(lat) != dims[2]) {
    stop("Parameter 'lat' must have as many elements as the number of cells along longitudes in the input array 'var'.")
  }

  # Prepare sorted lon/lat and other arguments
  latb <- sort(lat, index.return = TRUE)
  lonb <- sort(lon, index.return = TRUE)
  latmin <- floor(min(lat) / 10) * 10
  latmax <- ceiling(max(lat) / 10) * 10
  lonmin <- floor(min(lon) / 10) * 10
  lonmax <- ceiling(max(lon) / 10) * 10

  # Check latlims
  if (!is.numeric(latlims) || length(latlims) != 2) {
    stop("Parameter 'latlims' must be a numeric vector with two elements.")
  }
  latlims <- sort(latlims)
  center_at <- 90 * sign(latlims[which.max(abs(latlims))])
  if (max(abs(latlims - center_at)) > 90 + 20) {
    stop("The range specified in 'latlims' is too wide. 110 degrees supported maximum.")
  }
  dlon <- median(lonb$x[2:dims[1]] - lonb$x[1:(dims[1] - 1)]) / 2
  dlat <- median(latb$x[2:dims[2]] - latb$x[1:(dims[2] - 1)]) / 2
  original_last_lat <- latlims[which.min(abs(latlims))]
  last_lat <- latb$x[which.min(abs(latb$x - original_last_lat))] - dlat * sign(center_at)
  latlims[which.min(abs(latlims))] <- last_lat

  # Subset lat by latlims
  lat_plot_ind <- which(lat >= latlims[1] & lat <= latlims[2])
  latb_plot_ind <- which(latb$x >= latlims[1] & latb$x <= latlims[2])

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

  # Check: brks, cols, subsampleg, bar_limits, color_fun, bar_extra_labels, draw_bar_ticks
  #        draw_separators, triangle_ends_scale, label_scale, units, units_scale, 
  #        bar_label_digits
  # Build: brks, cols, bar_limits, col_inf, col_sup
  var_limits <- c(min(var, na.rm = TRUE), max(var, na.rm = TRUE))
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

  # Check filled.continents
  if (!.IsColor(filled.continents) && !is.logical(filled.continents)) {
    stop("Parameter 'filled.continents' must be logical or a colour identifier.")
  } else if (!is.logical(filled.continents)) {
    continent_color <- filled.continents
    filled.continents <- TRUE
  } else if (filled.continents) {
    continent_color <- gray(0.5)
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
  # Check contours
  if (!is.null(contours)) {
    if (!is.array(contours)) {
      stop("Parameter 'contours' must be a numeric array.")
    }
    if (length(dim(contours)) > 2) {
      contours <- drop(contours)
      dim(contours) <- head(c(dim(contours), 1, 1), 2)
    }
    if (length(dim(contours)) > 2) {
      stop("Parameter 'contours' must be a numeric array with two dimensions.")
    } else if (length(dim(contours)) < 2) {
      stop("Parameter 'contours' must be a numeric array with two dimensions.")
    }
    # Transpose the input matrices because the base plot functions work directly 
    # with dimensions c(lon, lat).
    if (dim(contours)[1] == dims[2] & dim(contours)[2] == dims[1]) {
      contours <- t(contours)
    } else {
      stop("Parameter 'contours' must have the same number of longitudes and latitudes as 'var'.")
    }
  }

  # Check brks2
  if (!is.null(contours)) {
    if (is.null(brks2)) {
      ll <- signif(min(contours, na.rm = TRUE), 2)
      ul <- signif(max(contours, na.rm = TRUE), 2)
      brks2 <- unique(signif(seq(ll, ul, length.out = length(brks)), 2))

    } else if (is.numeric(brks2) & length(brks2) == 1) {
      ll <- signif(min(contours, na.rm = TRUE), 2)
      ul <- signif(max(contours, na.rm = TRUE), 2)
      brks2 <- unique(signif(seq(ll, ul, length.out = brks2), 2))
    } else if (!is.numeric(brks2)) {
      stop("Parameter 'brks2' must be a numeric value or vector.")
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

  # Check contour_label_draw
  if (!is.logical(contour_label_draw)) {
    stop("Parameter 'contour_label_draw' must be a logical value.")
  }

  # Check contour_label_scale
  if (!is.numeric(contour_label_scale)) {
    stop("Parameter 'contour_label_scale' must be numeric.")
  }

  # Check dots, dot_symbol and dot_size
  if (!is.null(dots)) {
    if (dim(dots)[2] != dims[1] || dim(dots)[3] != dims[2]) {
      stop("Parameter 'dots' must have the same number of longitudes and latitudes as 'var'.")
    }
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

  # Check intlat
  if (!is.numeric(intlat)) {
    stop("Parameter 'intlat' must be numeric.")
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

  # Check numbfig
  if (!is.null(numbfig)) {
    if (!is.numeric(numbfig)) {
      stop("Parameter 'numbfig' must be numeric.")
    } else {
      numbfig <- round(numbfig)
      scale <- 1 / numbfig ** 0.3
      title_scale <- title_scale * scale
      margin_scale <- margin_scale * scale
      dot_size <- dot_size * scale
      arr_scale <- arr_scale * scale
      contour_label_scale <- contour_label_scale * scale
      contour_lwd <- contour_lwd * scale
    }
  } 

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
  margins <- rep(0.2, 4) * margin_scale
  cex_title <- 2 * title_scale
  if (toptitle != '') {
    margins[3] <- margins[3] + cex_title + 1
  }
  bar_extra_margin[1] <- bar_extra_margin[1] + margins[1]
  bar_extra_margin[3] <- bar_extra_margin[3] + margins[3]

  if (!is.null(varu)) {
    margins[1] <- margins[1] + 2.2 * units_scale
  }

  if (drawleg) {
    layout(matrix(1:2, ncol = 2, nrow = 1), widths = c(8, 2))
  }
  # Load the user parameters
  par(userArgs)
  par(mar = margins, las = 0)
  coast <- map("world", interior = FALSE, projection = "stereographic", 
               orientation = c(center_at, 0, 0), fill = filled.continents,
               xlim = c(-180,180), ylim = latlims, wrap = TRUE, plot = FALSE)
  # Compute the bounding circle
  limit <- abs(mapproj::mapproject(0, last_lat, projection = 'stereographic',
                                   orientation = c(center_at, 0, 0))$y)
  for (i in 1:length(coast$x)) {
    distance <- sqrt(coast$x[i]**2 + coast$y[i]**2)
    if (!is.na(distance)) {
      if (distance > limit) {
        coast$x[i] <- coast$x[i] / distance * limit
        coast$y[i] <- coast$y[i] / distance * limit
      }
    }
  }
  xcircle <- c()
  ycircle <- c()
  for (i in 0:500) {
    xcircle <- c(xcircle, sin(2 * pi / 500 * i) * limit)
    ycircle <- c(ycircle, cos(2 * pi / 500 * i) * limit)
  }
  circle <- list(x = xcircle, y = ycircle)
  # Plot circle to set up device
  plot(circle, type= 'l', axes = FALSE, lwd = 1, col = gray(0.2), asp = 1,
       xlab = '', ylab = '', main = toptitle, cex.main = cex_title)
  col_inf_image <- ifelse(is.null(col_inf), colNA, col_inf)
  col_sup_image <- ifelse(is.null(col_sup), colNA, col_sup)
  # Draw the data polygons
  for (jx in 1:dims[1]) { 
    for (jy in 1:length(lat_plot_ind)) {
      coord <- mapproj::mapproject(c(lon[jx] - dlon, lon[jx] + dlon,
                                     lon[jx] + dlon, lon[jx] - dlon),
                                   c(lat[lat_plot_ind][jy] - dlat, lat[lat_plot_ind][jy] - dlat,
                                     lat[lat_plot_ind][jy] + dlat, lat[lat_plot_ind][jy] + dlat))
      if (is.na(var[jx, lat_plot_ind[jy]] > 0)) {
        col <- colNA
      } else if (var[jx, lat_plot_ind[jy]] <= brks[1]) {
        col <- col_inf_image
      } else if (var[jx, lat_plot_ind[jy]] >= tail(brks, 1)) {
        col <- col_sup_image
      } else {
        ind <- which(brks[-1] >= var[jx, lat_plot_ind[jy]] & var[jx, lat_plot_ind[jy]] > brks[-length(brks)])
        col <- cols[ind]
      }
      polygon(coord, col = col, border = NA)
    }
  }

  # contours
  if (!is.null(contours)) {
    nbrks2 <- length(brks2)
    for (n_brks2 in 1:nbrks2) {
      cl <- grDevices::contourLines(x = lonb$x, y = latb$x[latb_plot_ind], 
                                    z = contours[lonb$ix, latb$ix[latb_plot_ind]],
                                    levels = brks2[n_brks2])
      if (length(cl) > 0) {
        for (i in seq_along(cl)) {
          xy <- mapproj::mapproject(cl[[i]]$x, cl[[i]]$y)
          xc <- xy$x
          yc <- xy$y
          nc <- length(xc)
          lines(xc, yc, col = contour_color, lwd = contour_lwd, lty = contour_lty)

          # draw label
          if (contour_label_draw) {
            label_char <- as.character(signif(brks2[n_brks2], 2))
            ## Check if the label has enough space to draw first.
            last_slope <- Inf
            put_label <- FALSE
            for (p1 in 1:nc) {
              p2 <- p1
              while (p2 < nc) {
                dist <- sqrt((yc[p2] - yc[p1])^2 + (xc[p2] - xc[p1])^2)
                if (!is.infinite(dist) &
                    dist > 1.2 * strwidth(label_char, cex = contour_label_scale)) {
                  put_label <- TRUE
                  slope <- (yc[p2] - yc[p1]) / (xc[p2] - xc[p1])
                  # flatter is better
                  if (abs(slope) < abs(last_slope)) {
                    last_slope <- slope
                    last_p1 <- p1
                    last_p2 <- p2
                  }
                  break  # Found a proper space for label. Move to the next p1.
                }
                p2 <- p2 + 1  # If the dist is not enough, try next p2.
              }
            }

            ## If label can be put
            if (put_label) {
              # Label should be at the middle of p1 and p2
              p_label <- (last_p1 + last_p2) / 2
              # string rotation angle is calculated from the slope
              srt_label <- atan(last_slope) * 57.2958  # radian to degree

              #NOTE: 'cex' in text() is the scale factor. The actual size will be 
              #      contour_label_scale * par("cex")
              text(xc[p_label], yc[p_label], label_char,
                   cex = contour_label_scale, col = contour_color, srt = srt_label)
            }
          }
        }
      }
    }
  }

  # Draw the dots
  if (!is.null(dots)) {
    numbfig <- 1  # for compatibility with PlotEquiMap code
    dots <- dots[, , lat_plot_ind, drop = FALSE]
    data_avail <- !is.na(var[, lat_plot_ind, drop = FALSE])
    for (counter in 1:(dim(dots)[1])) {
      points <- which(dots[counter, , ] & data_avail, arr.ind = TRUE)
      points_proj <- mapproj::mapproject(lon[points[, 1]], lat[lat_plot_ind][points[, 2]])
      points(points_proj$x, points_proj$y,
             pch = dot_symbol[counter],
             cex = dot_size[counter] * 3 / sqrt(sqrt(sum(lat >= latlims[which.min(abs(latlims))]) * length(lon))),
             lwd = dot_size[counter] * 3 / sqrt(sqrt(sum(lat >= latlims[which.min(abs(latlims))]) * length(lon))))
    }
  }

  # Draw the continents, grid and bounding circle
  if (filled.continents) {
    old_lwd <- par('lwd')
    par(lwd = coast_width)
    polygon(coast, col = continent_color, border = coast_color)
    par(lwd = old_lwd)
  } else {
    lines(coast, col = coast_color, lwd = coast_width)
  }
  mapproj::map.grid(lim = c(-180, 180, latlims), nx = 18, 
                    ny = ceiling((latlims[2] - latlims[1]) / intlat),
                    col = 'lightgrey', labels = FALSE) 
  polygon(circle, border = 'black')
  # Draw boxes on the map
  if (!is.null(boxlim)) {
    counter <- 1
    for (box in boxlim) {
      if (box[1] > box[3]) {
        box[1] <- box[1] - 360
      }
      if (length(box) != 4) {
        stop(paste("The", counter, "st box defined in the parameter 'boxlim' is ill defined."))
      } else if (center_at == 90 && (box[2] < original_last_lat || 
                                     box[4] > center_at) ||
                 center_at == -90 && (box[4] > original_last_lat ||
                                      box[2] < center_at)) {
        stop(paste("The limits of the", counter, 
                   "st box defined in the parameter 'boxlim' are invalid."))
      } else {
        mapproj::map.grid(lim = c(box[1], box[3], box[2], box[4]), 
                          nx = 2, ny = 2, pretty = FALSE, 
                          col = boxcol[counter], lty = "solid",
                          lwd = boxlwd[counter], labels = FALSE)
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
    lontab <- InsertDim(lonb$x, 2, length(latb$x[latb_plot_ind]), name = 'lat')
    lattab <- InsertDim(latb$x[latb_plot_ind], 1, length(lonb$x), name = 'lon')
    # Select a subsample of the points to an arrow for each "subsample" grid point
    # latmin has the most arrows, and latmax (polar point) has no arrow.
    sublon_max <- seq(1, length(lonb$x), arr_subsamp)
    sublat_max <- seq(1, length(latb$x[latb_plot_ind]), arr_subsamp)
    ## calculate the length of sublon for each lat
    arr_num_at_lat <- round(seq(length(sublon_max), 0, length.out = length(lat[lat_plot_ind])))
    ## If south hemisphere, revserse arr_num_at_lat (smaller lat has less arrows)
    if (center_at < 0) {
      arr_num_at_lat <- rev(arr_num_at_lat)
    }
    for (n_lat in seq_along(sublat_max)) {
      sublat <- sublat_max[n_lat]
      if (arr_num_at_lat[sublat] != 0) {
        sublon <- round(seq(1, length(lon), length.out = arr_num_at_lat[sublat]))
        # end points (start points + varu/varv)
        uaux <- lontab[sublon, sublat] + varu[lonb$ix, latb$ix[latb_plot_ind]][sublon, sublat] * 0.5 * arr_scale
        vaux <- lattab[sublon, sublat] + varv[lonb$ix, latb$ix[latb_plot_ind]][sublon, sublat] * 0.5 * arr_scale
  
        # project the start and end points on stereographic
        xy0 <- mapproj::mapproject(lontab[sublon, sublat], lattab[sublon, sublat])
        xy1 <- mapproj::mapproject(uaux, vaux)
        xc0 <- xy0$x
        yc0 <- xy0$y
        xc1 <- xy1$x
        yc1 <- xy1$y
        nc <- length(xc0)
  
        lenshaft <- 0.18 * arr_scale * arr_scale_shaft
        angleshaft <- 12 * arr_scale_shaft_angle
  
        # Plot Wind
        arrows(xc0, yc0,
               xc1, yc1,
               angle = angleshaft,
               length = lenshaft)
      }
    }

    # Plot an arrow at the bottom of the plot for the legend
    # Put arrow at lon = 0, lat = lowest lat (i.e., biggest circle) - (latmax - latmin)/8
    delta_arr_lengend <- (0.5 * arr_scale * arr_ref_len)
    posarlon <- c(0 - delta_arr_lengend / 2, 0 + delta_arr_lengend / 2) 
    posarlat <- rep(min(abs(lat[lat_plot_ind])) - diff(range(lat[lat_plot_ind]))/8, 2)
#NOTE: The following lines put legend at bottom left corner. But it's hard to put it horizontal
#    delta_arr_lengend <- (0.5 * arr_scale * arr_ref_len)/sqrt(2)
#    posarlat[1] <- posarlat[1] - delta_arr_lengend / 2
#    posarlat[2] <- posarlat[2] + delta_arr_lengend / 2
    ## turn into stereographic
    arr_lengend <- mapproj::mapproject(posarlon, posarlat)

    arrows(arr_lengend$x[1], arr_lengend$y[1],
           arr_lengend$x[2], arr_lengend$y[2],
           length = lenshaft, angle = angleshaft,
           xpd = TRUE)
    #save the parameter value
    xpdsave <- par('xpd')
    #desactivate xpd to be able to plot in margen
    par(xpd = NA)
    #plot text
    mtext(paste(as.character(arr_ref_len), arr_units, sep = ""),
          line = min(arr_lengend$y) + 1.8 * abs(min(arr_lengend$y)), 
          side = 1,
          at = mean(arr_lengend$x),
          cex = units_scale)
    #come back to the previous xpd value
    par(xpd = xpdsave)

  }


  #
  #  Colorbar
  # ~~~~~~~~~~
  #
  if (drawleg) {
    ColorBar(brks, cols, TRUE, subsampleg, bar_limits, var_limits,
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
