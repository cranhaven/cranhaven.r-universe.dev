#'Plot Maps of Most Likely Quantiles
#'
#'@author Veronica Torralba, \email{veronica.torralba@bsc.es}, Nicolau Manubens, 
#'\email{nicolau.manubens@bsc.es}
#'@description This function receives as main input (via the parameter 
#'\code{probs}) a collection of longitude-latitude maps, each containing the 
#'probabilities (from 0 to 1) of the different grid cells of belonging to a 
#'category. As many categories as maps provided as inputs are understood to 
#'exist. The maps of probabilities must be provided on a common rectangular 
#'regular grid, and a vector with the longitudes and a vector with the latitudes 
#'of the grid must be provided. The input maps can be provided in two forms, 
#'either as a list of multiple two-dimensional arrays (one for each category) or 
#'as a three-dimensional array, where one of the dimensions corresponds to the 
#'different categories.
#'
#'@param probs A list of bi-dimensional arrays with the named dimensions 
#'  'latitude' (or 'lat') and 'longitude' (or 'lon'), with equal size and in the 
#'  same order, or a single tri-dimensional array with an additional dimension 
#'  (e.g. 'bin') for the different categories. The arrays must contain 
#'  probability values between 0 and 1, and the probabilities for all categories 
#'  of a grid cell should not exceed 1 when added.
#'@param lon A numeric vector with the longitudes of the map grid, in the same 
#'  order as the values along the corresponding dimension in \code{probs}.
#'@param lat A numeric vector with the latitudes of the map grid, in the same 
#'  order as the values along the corresponding dimension in \code{probs}.
#'@param cat_dim The name of the dimension along which the different categories 
#'  are stored in \code{probs}. This only applies if \code{probs} is provided in 
#'  the form of 3-dimensional array. The default expected name is 'bin'.
#'@param bar_titles Vector of character strings with the names to be drawn on 
#'  top of the color bar for each of the categories. As many titles as 
#'  categories provided in \code{probs} must be provided.
#'@param col_unknown_cat Character string with a colour representation of the 
#'  colour to be used to paint the cells for which no category can be clearly 
#'  assigned. Takes the value 'white' by default.
#'@param drawleg Where to draw the common colour bar. Can take values TRUE, 
#'  FALSE or:\cr
#'  'up', 'u', 'U', 'top', 't', 'T', 'north', 'n', 'N'\cr
#'  'down', 'd', 'D', 'bottom', 'b', 'B', 'south', 's', 'S' (default)\cr
#'  'right', 'r', 'R', 'east', 'e', 'E'\cr
#'  'left', 'l', 'L', 'west', 'w', 'W'
#'@param ... Additional parameters to be sent to \code{PlotCombinedMap} and 
#'  \code{PlotEquiMap}.
#'@seealso \code{PlotCombinedMap} and \code{PlotEquiMap}
#'@examples
#'# Simple example
#'x <- array(1:(20 * 10), dim = c(lat = 10, lon = 20)) / 200
#'a <- x * 0.6
#'b <- (1 - x) * 0.6
#'c <- 1 - (a + b)
#'lons <- seq(0, 359.5, length = 20)
#'lats <- seq(-89.5, 89.5, length = 10)
#'\dontrun{
#'PlotMostLikelyQuantileMap(list(a, b, c), lons, lats, 
#'                          toptitle = 'Most likely tercile map',
#'                          bar_titles = paste('% of belonging to', c('a', 'b', 'c')), 
#'                          brks = 20, width = 10, height = 8)
#'}
#'
#'# More complex example
#'n_lons <- 40
#'n_lats <- 20
#'n_timesteps <- 100
#'n_bins <- 4
#'
#'# 1. Generation of sample data
#'lons <- seq(0, 359.5, length = n_lons)
#'lats <- seq(-89.5, 89.5, length = n_lats)
#'
#'# This function builds a 3-D gaussian at a specified point in the map.
#'make_gaussian <- function(lon, sd_lon, lat, sd_lat) {
#'  w <- outer(lons, lats, function(x, y) dnorm(x, lon, sd_lon) * dnorm(y, lat, sd_lat))
#'  min_w <- min(w)
#'  w <- w - min_w
#'  w <- w / max(w)
#'  w <- t(w)
#'  names(dim(w)) <- c('lat', 'lon')
#'  w
#'}
#'
#'# This function generates random time series (with values ranging 1 to 5) 
#'# according to 2 input weights.
#'gen_data <- function(w1, w2, n) {
#'  r <- sample(1:5, n, 
#'              prob = c(.05, .9 * w1, .05, .05, .9 * w2), 
#'              replace = TRUE)
#'  r <- r + runif(n, -0.5, 0.5)
#'  dim(r) <- c(time = n)
#'  r
#'}
#'
#'# We build two 3-D gaussians.
#'w1 <- make_gaussian(120, 80, 20, 30)
#'w2 <- make_gaussian(260, 60, -10, 40)
#'
#'# We generate sample data (with dimensions time, lat, lon) according 
#'# to the generated gaussians
#'sample_data <- multiApply::Apply(list(w1, w2), NULL, 
#'                                 gen_data, n = n_timesteps)$output1
#'
#'# 2. Binning sample data
#'prob_thresholds <- 1:n_bins / n_bins
#'prob_thresholds <- prob_thresholds[1:(n_bins - 1)]
#'thresholds <- quantile(sample_data, prob_thresholds)
#'
#'binning <- function(x, thresholds) {
#'  n_samples <- length(x)
#'  n_bins <- length(thresholds) + 1
#'
#'  thresholds <- c(thresholds, max(x))
#'  result <- 1:n_bins
#'  lower_threshold <- min(x) - 1
#'  for (i in 1:n_bins) {
#'    result[i] <- sum(x > lower_threshold & x <= thresholds[i]) / n_samples
#'    lower_threshold <- thresholds[i]
#'  }
#'
#'  dim(result) <- c(bin = n_bins)
#'  result
#'}
#'
#'bins <- multiApply::Apply(sample_data, 'time', binning, thresholds)$output1
#'
#'# 3. Plotting most likely quantile/bin
#'\dontrun{
#'PlotMostLikelyQuantileMap(bins, lons, lats, 
#'                          toptitle = 'Most likely quantile map',
#'                          bar_titles = paste('% of belonging to', letters[1:n_bins]),
#'                          mask = 1 - (w1 + w2 / max(c(w1, w2))), 
#'                          brks = 20, width = 10, height = 8)
#'}
#'@importFrom maps map 
#'@importFrom graphics box image layout mtext par plot.new
#'@importFrom grDevices adjustcolor bmp colorRampPalette dev.cur dev.new dev.off hcl jpeg pdf png postscript svg tiff
#'@export
PlotMostLikelyQuantileMap <- function(probs, lon, lat, cat_dim = 'bin',
                                      bar_titles = NULL, 
                                      col_unknown_cat = 'white', drawleg = T,
                                      ...) {
  # Check probs
  error <- FALSE
  if (is.list(probs)) {
    if (length(probs) < 1) {
      stop("Parameter 'probs' must be of length >= 1 if provided as a list.")
    }
    check_fun <- function(x) {
      is.numeric(x) && (length(dim(x)) == 2)
    }
    if (!all(sapply(probs, check_fun))) {
      error <- TRUE
    }
    ref_dims <- dim(probs[[1]])
    equal_dims <- all(sapply(probs, function(x) identical(dim(x), ref_dims)))
    if (!equal_dims) {
      stop("All arrays in parameter 'probs' must have the same dimension ",
           "sizes and names when 'probs' is provided as a list of arrays.")
    }
    num_probs <- length(probs)
    probs <- unlist(probs)
    dim(probs) <- c(ref_dims, map = num_probs)
    cat_dim <- 'map'
  }
  if (!is.numeric(probs)) {
    error <- TRUE
  }
  if (is.null(dim(probs))) {
    error <- TRUE
  }
  if (length(dim(probs)) != 3) {
    error <- TRUE
  }
  if (error) {
    stop("Parameter 'probs' must be either a numeric array with 3 dimensions ",
         " or a list of numeric arrays of the same size with the 'lon' and ",
         "'lat' dimensions.")
  }
  dimnames <- names(dim(probs))
  
  # Check cat_dim
  if (is.character(cat_dim)) {
    if (is.null(dimnames)) {
      stop("Specified a dimension name in 'cat_dim' but no dimension names provided ",
           "in 'probs'.")
    }
    cat_dim <- which(dimnames == cat_dim)
    if (length(cat_dim) < 1) {
      stop("Dimension 'cat_dim' not found in 'probs'.")
    }
    cat_dim <- cat_dim[1]
  } else if (!is.numeric(cat_dim)) {
    stop("Parameter 'cat_dim' must be either a numeric value or a ",
         "dimension name.")
  }
  if (length(cat_dim) != 1) {
    stop("Parameter 'cat_dim' must be of length 1.")
  }
  cat_dim <- round(cat_dim)
  nprobs <- dim(probs)[cat_dim]
  
  # Check bar_titles
  if (is.null(bar_titles)) {
    if (nprobs == 3) {
      bar_titles <- c("Below normal (%)", "Normal (%)", "Above normal (%)")
    } else if (nprobs == 5) {
      bar_titles <- c("Low (%)", "Below normal (%)", 
                         "Normal (%)", "Above normal (%)", "High (%)")
    } else {
      bar_titles <- paste0("Cat. ", 1:nprobs, " (%)")
    }
  }
  
  minimum_value <- ceiling(1 / nprobs * 10 * 1.1) * 10

  # By now, the PlotCombinedMap function is included below in this file. 
  # In the future, PlotCombinedMap will be part of s2dverification and will 
  # be properly imported.
  PlotCombinedMap(probs * 100, lon, lat, map_select_fun = max, 
                  display_range = c(minimum_value, 100),
                  map_dim = cat_dim, 
                  bar_titles = bar_titles,
                  col_unknown_map = col_unknown_cat, 
                  drawleg = drawleg, ...)
}
