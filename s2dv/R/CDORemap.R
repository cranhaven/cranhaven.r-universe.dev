#'Interpolate arrays with longitude and latitude dimensions using CDO
#'
#'This function takes as inputs a multidimensional array (optional), a vector 
#'or matrix of longitudes, a vector or matrix of latitudes, a destination grid 
#'specification, and the name of a method to be used to interpolate (one of 
#'those available in the 'remap' utility in CDO). The interpolated array is 
#'returned (if provided) together with the new longitudes and latitudes.\cr\cr 
#'\code{CDORemap()} permutes by default the dimensions of the input array (if 
#'needed), splits it in chunks (CDO can work with data arrays of up to 4 
#'dimensions), generates a file with the data of each chunk, interpolates it 
#'with CDO, reads it back into R and merges it into a result array. If no 
#'input array is provided, the longitude and latitude vectors will be 
#'transformed only. If the array is already on the desired destination grid, 
#'no transformation is performed (this behvaiour works only for lonlat and 
#'gaussian grids). \cr\cr
#'Any metadata attached to the input data array, longitudes or latitudes will 
#'be preserved or accordingly modified.
#'
#'@param data_array Multidimensional numeric array to be interpolated. If 
#'  provided, it must have at least a longitude and a latitude dimensions, 
#'  identified by the array dimension names. The names for these dimensions 
#'  must be one of the recognized by s2dverification (can be checked with 
#'  \code{s2dv:::.KnownLonNames()} and \code{s2dv:::.KnownLatNames()}).
#'@param lons Numeric vector or array of longitudes of the centers of the grid 
#'  cells. Its size must match the size of the longitude/latitude dimensions 
#'  of the input array.
#'@param lats Numeric vector or array of latitudes of the centers of the grid 
#'  cells. Its size must match the size of the longitude/latitude dimensions 
#'  of the input array.
#'@param grid Character string specifying either a name of a target grid 
#'  (recognized by CDO; e.g.: 'r256x128', 't106grid') or a path to another 
#'  NetCDF file which to read the target grid from (a single grid must be 
#'  defined in such file).
#'@param method Character string specifying an interpolation method 
#'  (recognized by CDO; e.g.: 'con', 'bil', 'bic', 'dis', 'con2', 'laf', 'nn').
#'  The following long names are also supported: 'conservative', 'bilinear', 
#'  'bicubic' and 'distance-weighted'.
#'@param avoid_writes The step of permutation is needed when the input array 
#'  has more than 3 dimensions and none of the longitude or latitude dimensions
#'   in the right-most position (CDO would not accept it without permuting 
#'  previously). This step, executed by default when needed, can be avoided 
#'  for the price of writing more intermediate files (whis usually is 
#'  unconvenient) by setting the parameter \code{avoid_writes = TRUE}.
#'@param crop Whether to crop the data after interpolation with 
#'  'cdo sellonlatbox' (TRUE) or to extend interpolated data to the whole
#'  world as CDO does by default (FALSE). The default value is TRUE.\cr
#'  \itemize{
#'    \item{
#'      If \code{crop = TRUE}, the longitude and latitude borders to be cropped
#'      at are taken as the limits of the cells at the borders (not the values
#'      of 'lons' and 'lats', which are perceived as cell centers), i.e., the
#'      resulting array will contain data that covers the same area as the input
#'      array. This is equivalent to specifying \code{crop = 'preserve'}, i.e.,
#'      preserving area. Notice that the longitude range of returning array will
#'      follow the original data 'lons' instead of the target grid 'grid'.
#'    }
#'    \item{
#'      If \code{crop = FALSE}, the returning array is not cropped, i.e., a 
#'      global domain, and the longitude range will be the same as the target
#'      grid 'grid'.
#'    }
#'    \item{
#'      If \code{crop = 'tight'}, the borders to be cropped at are taken as the 
#'      minimum and maximum cell centers in 'lons' and 'lats', i.e., the area
#'      covered by the resulting array may be smaller if interpolating from a 
#'      coarse grid to a fine grid. 
#'    }
#'    \item{
#'      The parameter 'crop' also accepts a numeric vector of customized borders
#'      to be cropped at:\cr
#'      c(western border, eastern border, southern border, northern border).
#'    }
#'  }
#'@param force_remap Whether to force remapping, even if the input data array 
#'  is already on the target grid.
#'@param write_dir Path to the directory where to create the intermediate 
#'  files for CDO to work. By default, the R session temporary directory is 
#'  used (\code{tempdir()}).
#'@param print_sys_msg A logical value indicating to print the messages from 
#'  system CDO commands. The default is FALSE to keep function using clean.
#'@param ncores An integer indicating the number of theads used for 
#'  interpolation (i.e., \code{-P} in cdo command.) The default value is NULL
#'  and \code{-P} is not used.
#'
#'@return A list with the following components:
#'  \item{'data_array'}{The interpolated data array (if an input array 
#'  is provided at all, NULL otherwise).}
#'  \item{'lons'}{The longitudes of the data on the destination grid.}
#'  \item{'lats'}{The latitudes of the data on the destination grid.}
#'@examples
#'  \dontrun{
#'# Interpolating only vectors of longitudes and latitudes
#'lon <- seq(0, 360 - 360/50, length.out = 50)
#'lat <- seq(-90, 90, length.out = 25)
#'tas2 <- CDORemap(NULL, lon, lat, 't170grid', 'bil', TRUE)
#'
#'# Minimal array interpolation
#'tas <- array(1:50, dim = c(25, 50))
#'names(dim(tas)) <- c('lat', 'lon')
#'lon <- seq(0, 360 - 360/50, length.out = 50)
#'lat <- seq(-90, 90, length.out = 25)
#'tas2 <- CDORemap(tas, lon, lat, 't170grid', 'bil', TRUE)
#'
#'# Metadata can be attached to the inputs. It will be preserved and 
#'# accordignly modified.
#'tas <- array(1:50, dim = c(25, 50))
#'names(dim(tas)) <- c('lat', 'lon')
#'lon <- seq(0, 360 - 360/50, length.out = 50)
#'metadata <- list(lon = list(units = 'degrees_east'))
#'attr(lon, 'variables') <- metadata
#'lat <- seq(-90, 90, length.out = 25)
#'metadata <- list(lat = list(units = 'degrees_north'))
#'attr(lat, 'variables') <- metadata
#'metadata <- list(tas = list(dim = list(lat = list(len = 25,
#'                                                  vals = lat),
#'                                       lon = list(len = 50,
#'                                                  vals = lon)
#'                                      )))
#'attr(tas, 'variables') <- metadata
#'tas2 <- CDORemap(tas, lon, lat, 't170grid', 'bil', TRUE)
#'
#'# Arrays of any number of dimensions in any order can be provided.
#'num_lats <- 25
#'num_lons <- 50
#'tas <- array(1:(10*num_lats*10*num_lons*10), 
#'             dim = c(10, num_lats, 10, num_lons, 10))
#'names(dim(tas)) <- c('a', 'lat', 'b', 'lon', 'c')
#'lon <- seq(0, 360 - 360/num_lons, length.out = num_lons)
#'metadata <- list(lon = list(units = 'degrees_east'))
#'attr(lon, 'variables') <- metadata
#'lat <- seq(-90, 90, length.out = num_lats)
#'metadata <- list(lat = list(units = 'degrees_north'))
#'attr(lat, 'variables') <- metadata
#'metadata <- list(tas = list(dim = list(a = list(),
#'                                       lat = list(len = num_lats,
#'                                                  vals = lat),
#'                                       b = list(),
#'                                       lon = list(len = num_lons,
#'                                                  vals = lon),
#'                                       c = list()
#'                                      )))
#'attr(tas, 'variables') <- metadata
#'tas2 <- CDORemap(tas, lon, lat, 't17grid', 'bil', TRUE)
#'# The step of permutation can be avoided but more intermediate file writes
#'# will be performed.
#'tas2 <- CDORemap(tas, lon, lat, 't17grid', 'bil', FALSE)
#'
#'# If the provided array has the longitude or latitude dimension in the 
#'# right-most position, the same number of file writes will be performed,
#'# even if avoid_wrties = FALSE.
#'num_lats <- 25
#'num_lons <- 50
#'tas <- array(1:(10*num_lats*10*num_lons*10), 
#'             dim = c(10, num_lats, 10, num_lons))
#'names(dim(tas)) <- c('a', 'lat', 'b', 'lon')
#'lon <- seq(0, 360 - 360/num_lons, length.out = num_lons)
#'metadata <- list(lon = list(units = 'degrees_east'))
#'attr(lon, 'variables') <- metadata
#'lat <- seq(-90, 90, length.out = num_lats)
#'metadata <- list(lat = list(units = 'degrees_north'))
#'attr(lat, 'variables') <- metadata
#'metadata <- list(tas = list(dim = list(a = list(),
#'                                       lat = list(len = num_lats,
#'                                                  vals = lat),
#'                                       b = list(),
#'                                       lon = list(len = num_lons,
#'                                                  vals = lon)
#'                                      )))
#'attr(tas, 'variables') <- metadata
#'tas2 <- CDORemap(tas, lon, lat, 't17grid', 'bil', TRUE)
#'tas2 <- CDORemap(tas, lon, lat, 't17grid', 'bil', FALSE)
#'
#'# An example of an interpolation from and onto a rectangular regular grid
#'num_lats <- 25
#'num_lons <- 50
#'tas <- array(1:(1*num_lats*num_lons), dim = c(num_lats, num_lons))
#'names(dim(tas)) <- c('y', 'x')
#'lon <- array(seq(0, 360 - 360/num_lons, length.out = num_lons), 
#'             dim = c(num_lons, num_lats))
#'metadata <- list(lon = list(units = 'degrees_east'))
#'names(dim(lon)) <- c('x', 'y')
#'attr(lon, 'variables') <- metadata
#'lat <- t(array(seq(-90, 90, length.out = num_lats), 
#'               dim = c(num_lats, num_lons)))
#'metadata <- list(lat = list(units = 'degrees_north'))
#'names(dim(lat)) <- c('x', 'y')
#'attr(lat, 'variables') <- metadata
#'tas2 <- CDORemap(tas, lon, lat, 'r100x50', 'bil')
#'
#'# An example of an interpolation from an irregular grid onto a gaussian grid
#'num_lats <- 25
#'num_lons <- 50
#'tas <- array(1:(10*num_lats*10*num_lons*10), 
#'             dim = c(10, num_lats, 10, num_lons))
#'names(dim(tas)) <- c('a', 'j', 'b', 'i')
#'lon <- array(seq(0, 360 - 360/num_lons, length.out = num_lons), 
#'             dim = c(num_lons, num_lats))
#'metadata <- list(lon = list(units = 'degrees_east'))
#'names(dim(lon)) <- c('i', 'j')
#'attr(lon, 'variables') <- metadata
#'lat <- t(array(seq(-90, 90, length.out = num_lats), 
#'         dim = c(num_lats, num_lons)))
#'metadata <- list(lat = list(units = 'degrees_north'))
#'names(dim(lat)) <- c('i', 'j')
#'attr(lat, 'variables') <- metadata
#'tas2 <- CDORemap(tas, lon, lat, 't17grid', 'bil')
#'
#'# Again, the dimensions can be in any order
#'num_lats <- 25
#'num_lons <- 50
#'tas <- array(1:(10*num_lats*10*num_lons), 
#'             dim = c(10, num_lats, 10, num_lons))
#'names(dim(tas)) <- c('a', 'j', 'b', 'i')
#'lon <- array(seq(0, 360 - 360/num_lons, length.out = num_lons), 
#'             dim = c(num_lons, num_lats))
#'names(dim(lon)) <- c('i', 'j')
#'lat <- t(array(seq(-90, 90, length.out = num_lats), 
#'               dim = c(num_lats, num_lons)))
#'names(dim(lat)) <- c('i', 'j')
#'tas2 <- CDORemap(tas, lon, lat, 't17grid', 'bil')
#'tas2 <- CDORemap(tas, lon, lat, 't17grid', 'bil', FALSE)
#'# It is ossible to specify an external NetCDF file as target grid reference
#'tas2 <- CDORemap(tas, lon, lat, 'external_file.nc', 'bil')
#'}
#'@import ncdf4
#'@importFrom easyNCDF ArrayToNc
#'@importFrom stats lm predict setNames 
#'@export
CDORemap <- function(data_array = NULL, lons, lats, grid, method, 
                     avoid_writes = TRUE, crop = TRUE,
                     force_remap = FALSE, write_dir = tempdir(),
                     print_sys_msg = FALSE,
                     ncores = NULL) {  #, mask = NULL) {
  .isRegularVector <- function(x, tol = 0.1) {
    if (length(x) < 2) {
      #stop("The provided vector must be of length 2 or greater.")
      TRUE
    } else {
      spaces <- x[2:length(x)] - x[1:(length(x) - 1)]
      (sum(abs(spaces - mean(spaces)) > mean(spaces) / (1 / tol)) < 2)
    }
  }
  # Check parameters data_array, lons and lats.
  known_lon_names <- .KnownLonNames()
  known_lat_names <- .KnownLatNames()
  if (!is.numeric(lons) || !is.numeric(lats)) {
    stop("Expected numeric 'lons' and 'lats'.")
  }
  if (anyNA(lons > 0)) {
    stop("Found invalid values in 'lons'.")
  }
  if (anyNA(lats > 0)) {
    stop("Found invalid values in 'lats'.")
  }
  if (is.null(dim(lons))) {
    dim(lons) <- length(lons)
  }
  if (is.null(dim(lats))) {
    dim(lats) <- length(lats)
  }
  if (length(dim(lons)) > 2 || length(dim(lats)) > 2) {
    stop("'lons' and 'lats' can only have up to 2 dimensions.")
  }
  if (length(dim(lons)) != length(dim(lats))) {
    stop("'lons' and 'lats' must have the same number of dimensions.")
  }
  if (length(dim(lons)) == 2 && !all(dim(lons) == dim(lats))) {
    stop("'lons' and 'lats' must have the same dimension sizes.")
  }
  return_array <- TRUE
  if (is.null(data_array)) {
    return_array <- FALSE
    if (length(dim(lons)) == 1) {
      array_dims <- c(length(lats), length(lons))
      new_lon_dim_name <- 'lon'
      new_lat_dim_name <- 'lat'

      if (!is.null(names(dim(lons)))) {
        if (any(known_lon_names %in% names(dim(lons)))) {
          new_lon_dim_name <- known_lon_names[which(known_lon_names %in% names(dim(lons)))[1]]
        }
      }
      if (!is.null(names(dim(lats)))) {
        if (any(known_lat_names %in% names(dim(lats)))) {
          new_lat_dim_name <- known_lat_names[which(known_lat_names %in% names(dim(lats)))[1]]
        }
      }
      names(array_dims) <- c(new_lat_dim_name, new_lon_dim_name)

    } else {  # irregular
      array_dims <- dim(lons)
      if (is.null(names(array_dims))) {
        new_lon_dim_name <- 'i'
        new_lat_dim_name <- 'j'
      }
    }

    data_array <- array(NA_real_, array_dims)
  }
  if (!(is.logical(data_array) || is.numeric(data_array)) || !is.array(data_array)) {
    stop("Parameter 'data_array' must be a numeric array.")
  }
  if (is.null(names(dim(data_array)))) {
    stop("Parameter 'data_array' must have named dimensions.")
  }
  lon_dim <- which(known_lon_names %in% names(dim(data_array)))
  if (length(lon_dim) < 1) {
    stop("Could not find a known longitude dimension name in the provided 'data_array'.")
  }
  if (length(lon_dim) > 1) {
    stop("Found more than one known longitude dimension names in the provided 'data_array'.")
  }
  lon_dim <- known_lon_names[lon_dim]
  lat_dim <- which(known_lat_names %in% names(dim(data_array)))
  if (length(lat_dim) < 1) {
    stop("Could not find a known latitude dimension name in the provided 'data_array'.")
  }
  if (length(lat_dim) > 1) {
    stop("Found more than one known latitude dimension name in the provided 'data_array'.")
  }
  lat_dim <- known_lat_names[lat_dim]
  if (is.null(names(dim(lons)))) {
    if (length(dim(lons)) == 1) {
      names(dim(lons)) <- lon_dim
    } else {
      stop("Parameter 'lons' must be provided with dimension names.")
    }
  } else {
    if (!(lon_dim %in% names(dim(lons)))) {
      stop("Parameter 'lon' must have the same longitude dimension name as the 'data_array'.")
    }
    if (length(dim(lons)) > 1 && !(lat_dim %in% names(dim(lons)))) {
      stop("Parameter 'lon' must have the same latitude dimension name as the 'data_array'.")
    }
  }
  if (is.null(names(dim(lats)))) {
    if (length(dim(lats)) == 1) {
      names(dim(lats)) <- lat_dim
    } else {
      stop("Parameter 'lats' must be provided with dimension names.")
    }
  } else {
    if (!(lat_dim %in% names(dim(lats)))) {
      stop("Parameter 'lat' must have the same latitude dimension name as the 'data_array'.")
    }
    if (length(dim(lats)) > 1 && !(lon_dim %in% names(dim(lats)))) {
      stop("Parameter 'lat' must have the same longitude dimension name as the 'data_array'.")
    }
  }
  lons_attr_bk <- attributes(lons)
  if (is.null(lons_attr_bk)) {
    lons_attr_bk <- list()
  }
  lats_attr_bk <- attributes(lats)
  if (is.null(lats_attr_bk)) {
    lats_attr_bk <- list()
  }
  if (length(attr(lons, 'variables')) == 0) {
    new_metadata <- list(list())
    if (length(dim(lons)) == 1) {
      names(new_metadata) <- lon_dim
    } else {
      names(new_metadata) <- paste0(lon_dim, '_var')
    }
    attr(lons, 'variables') <- new_metadata
  }
  if (!('units' %in% names(attr(lons, 'variables')[[1]]))) {
    new_metadata <- attr(lons, 'variables')
    #names(new_metadata)[1] <- lon_dim
    new_metadata[[1]][['units']] <- 'degrees_east'
    attr(lons, 'variables') <- new_metadata
  }
  if (length(attr(lats, 'variables')) == 0) {
    new_metadata <- list(list())
    if (length(dim(lats)) == 1) {
      names(new_metadata) <- lat_dim
    } else {
      names(new_metadata) <- paste0(lat_dim, '_var')
    }
    attr(lats, 'variables') <- new_metadata
  }
  if (!('units' %in% names(attr(lats, 'variables')[[1]]))) {
    new_metadata <- attr(lats, 'variables')
    #names(new_metadata)[1] <- lat_dim
    new_metadata[[1]][['units']] <- 'degrees_north'
    attr(lats, 'variables') <- new_metadata
  }
  # Check grid.
  if (!is.character(grid)) {
    stop("Parameter 'grid' must be a character string specifying a ",
         "target CDO grid, 'rXxY' or 'tRESgrid', or a path to another ",
         "NetCDF file.")
  }
  if (grepl('^r[0-9]{1,}x[0-9]{1,}$', grid)) {
    grid_type <- 'regular'
    grid_lons <- as.numeric(strsplit(strsplit(grid, 'x')[[1]][1], 'r')[[1]][2])
    grid_lats <- as.numeric(strsplit(grid, 'x')[[1]][2])
  } else if (grepl('^t[0-9]{1,}grid$', grid)) {
    grid_type <- 'gaussian'
    grid_t <- as.numeric(strsplit(strsplit(grid, 'grid')[[1]][1], 't')[[1]][2])
    grid_size <- .t2nlatlon(grid_t)
    grid_lons <- grid_size[2]
    grid_lats <- grid_size[1]
  } else {
    grid_type <- 'custom'
  }
  # Check method.
  if (method %in% c('bil', 'bilinear')) {
    method <- 'bil'
  } else if (method %in% c('bic', 'bicubic')) {
    method <- 'bic'
  } else if (method %in% c('con', 'conservative')) {
    method <- 'con'
  } else if (method %in% c('dis', 'distance-weighted')) {
    method <- 'dis'
  } else if (method == 'nn') {
    method <- 'nn'
  } else if (method == 'laf') {
    method <- 'laf'
  } else if (method == 'con2') {
    method <- 'con2'
  } else {
    stop("Unsupported CDO remap method. Only 'bilinear', ",
         "'bicubic', 'conservative', 'distance-weighted', 'nn', ",
         "'laf', and 'con2' are supported.")
  }
  # Check avoid_writes
  if (!is.logical(avoid_writes)) {
    stop("Parameter 'avoid_writes' must be a logical value.")
  }
  # Check crop
  crop_tight <- FALSE
  if (is.character(crop)) {
    if (crop == 'tight') {
      crop_tight <- TRUE
    } else if (crop != 'preserve') {
      stop("Parameter 'crop' can only take the values 'tight' or 'preserve' ",
           "if specified as a character string.")
    }
    crop <- TRUE
  }
  if (is.logical(crop)) {
    if (crop) {
      if (length(lons) == 1 || length(lats) == 1) {
        stop("CDORemap cannot remap if crop = TRUE and values for only one ",
             "longitude or one latitude are provided. Either a) provide ",
             "values for more than one longitude/latitude, b) explicitly ",
             "specify the crop limits in the parameter crop, or c) set ",
             "crop = FALSE.")
      }
      if (crop_tight) {
        lon_extremes <- c(min(lons), max(lons))
        lat_extremes <- c(min(lats), max(lats))
      } else {
        # Here we are trying to look for the extreme lons and lats in the data.
        # Not the centers of the extreme cells, but the borders of the extreme cells.
###---
        if (length(dim(lons)) == 1) {
          tmp_lon <- lons
        } else {
          min_pos <- which(lons == min(lons), arr.ind = TRUE)[1, ]
          tmp_lon <- Subset(lons, lat_dim,
                            min_pos[which(names(dim(lons)) == lat_dim)],
                            drop = 'selected')
        }
        i <- seq_along(tmp_lon)
        degree <- min(3, length(i) - 1)
        lon_model <- lm(tmp_lon ~ poly(i, degree))
        lon_extremes <- c(NA, NA)
        left_is_min <- FALSE
        right_is_max <- FALSE
        if (which.min(tmp_lon) == 1) {
          left_is_min <- TRUE
          prev_lon <- predict(lon_model, data.frame(i = 0))
          first_lon_cell_width <- (tmp_lon[1] - prev_lon)
          # The signif is needed because cdo sellonlatbox crashes with too many digits
          lon_extremes[1] <- tmp_lon[1] - first_lon_cell_width / 2
        } else {
          next_lon <- predict(lon_model, data.frame(i = length(tmp_lon) + 1))
          last_lon_cell_width <- (next_lon - tmp_lon[length(tmp_lon)])
          lon_extremes[1] <- tmp_lon[length(tmp_lon)] + last_lon_cell_width / 2
        }
        if (which.max(tmp_lon) == length(tmp_lon)) {
          right_is_max <- TRUE
          next_lon <- predict(lon_model, data.frame(i = length(tmp_lon) + 1))
          last_lon_cell_width <- (next_lon - tmp_lon[length(tmp_lon)])
          lon_extremes[2] <- tmp_lon[length(tmp_lon)] + last_lon_cell_width / 2
        } else {
          prev_lon <- predict(lon_model, data.frame(i = 0))
          first_lon_cell_width <- (tmp_lon[1] - prev_lon)
          # The signif is needed because cdo sellonlatbox crashes with too many digits
          lon_extremes[2] <- tmp_lon[1] - first_lon_cell_width / 2
        }
        tolerance <- 1e-10
        lon_extremes <- round(lon_extremes, 10)
        # Adjust the crop window if possible in order to keep lons from 0 to 360 
        # or from -180 to 180 when the extremes of the cropped window are contiguous.
        if (right_is_max) {
          if (lon_extremes[1] < -180) {
            if (!((lon_extremes[2] < 180) && 
                !(abs((180 - lon_extremes[2]) - last_lon_cell_width / 2) <= tolerance))) {
              lon_extremes[1] <- -180
              lon_extremes[2] <- 180
            }
          } else if (lon_extremes[1] < 0) {
            if (!((lon_extremes[2] < 360) && 
                !(abs((360 - lon_extremes[2]) - last_lon_cell_width / 2) <= tolerance))) {
              lon_extremes[1] <- 0
              lon_extremes[2] <- 360
            }
          }
        } 
        if (left_is_min) {
          if (lon_extremes[2] > 360) {
            if (!((lon_extremes[1] > 0) && 
                !(abs(lon_extremes[1] - first_lon_cell_width / 2) <= tolerance))) {
              lon_extremes[1] <- 0
              lon_extremes[2] <- 360
            }
          } else if (lon_extremes[2] > 180) {
            if (!((lon_extremes[1] > -180) &&
                !(abs((180 + lon_extremes[1]) - first_lon_cell_width / 2) <= tolerance))) {
              lon_extremes[1] <- -180
              lon_extremes[2] <- 180
            }
          }
        } 
##      lon_extremes <- signif(lon_extremes, 5)
##      lon_extremes <- lon_extremes + 0.00001
###---
        if (length(dim(lats)) == 1) {
          tmp_lat <- lats
        } else {
          min_pos <- which(lats == min(lats), arr.ind = TRUE)[1, ]
          tmp_lat <- Subset(lats, lon_dim, min_pos[which(names(dim(lats)) == lon_dim)],
                            drop = 'selected')
        }
        i <- seq_along(tmp_lat)
        degree <- min(3, length(i) - 1)
        lat_model <- lm(tmp_lat ~ poly(i, degree))
        lat_extremes <- c(NA, NA)
        if (which.min(tmp_lat) == 1) {
          prev_lat <- predict(lat_model, data.frame(i = 0))
          lat_extremes[1] <- tmp_lat[1] - (tmp_lat[1] - prev_lat) / 2
        } else {
          next_lat <- predict(lat_model, data.frame(i = length(tmp_lat) + 1))
          lat_extremes[1] <- tmp_lat[length(tmp_lat)] + (next_lat - tmp_lat[length(tmp_lat)]) / 2
        }
        if (which.max(tmp_lat) == length(tmp_lat)) {
          next_lat <- predict(lat_model, data.frame(i = length(tmp_lat) + 1))
          lat_extremes[2] <- tmp_lat[length(tmp_lat)] + (next_lat - tmp_lat[length(tmp_lat)]) / 2
        } else {
          prev_lat <- predict(lat_model, data.frame(i = 0))
          lat_extremes[2] <- tmp_lat[1] - (tmp_lat[1] - prev_lat) / 2
        }
        lat_extremes <- round(lat_extremes, 10)
##      lat_extremes <- signif(lat_extremes, 5)
        # Adjust crop window
        if (lat_extremes[1] < -90) {
          lat_extremes[1] <- -90
        } else if (lat_extremes[1] > 90) {
          lat_extremes[1] <- 90
        }
        if (lat_extremes[2] < -90) {
          lat_extremes[2] <- -90
        } else if (lat_extremes[2] > 90) {
          lat_extremes[2] <- 90
        }
###---
      }
    }
  } else if (is.numeric(crop)) {
    if (length(crop) != 4) {
      stop("Paramrter 'crop' must be a logical value or a numeric vector of length 4: ",
           "c(western border, eastern border, southern border, northern border.")
    } else {
      lon_extremes <- crop[1:2]
      lat_extremes <- crop[3:4]
      crop <- TRUE
    }
  } else {
    stop("Parameter 'crop' must be a logical value or a numeric vector.")
  }
  # Check force_remap
  if (!is.logical(force_remap)) {
    stop("Parameter 'force_remap' must be a logical value.")
  }
  # Check write_dir
  if (!is.character(write_dir)) {
    stop("Parameter 'write_dir' must be a character string.")
  }
  if (!dir.exists(write_dir)) {
    stop("Parameter 'write_dir' must point to an existing directory.")
  }
  # Check ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | any(ncores %% 1 != 0) | any(ncores < 0) |
        length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }
#  if (!is.null(mask)) {
#    if (!is.numeric(mask) || !is.array(mask)) {
#      stop("Parameter 'mask' must be a numeric array.")
#    }
#    if (length(dim(mask)) != 2) {
#      stop("Parameter 'mask' must have two dimensions.")
#    }
#    if (is.null(names(dim(mask)))) {
#      if (dim(data_array)[lat_dim] == dim(data_array)[lon_dim]) {
#        stop("Cannot disambiguate which is the longitude dimension of ",
#             "the provided 'mask'. Provide it with dimension names.")
#      }
#      names(dim(mask)) <- c('', '')
#      found_lon_dim <- which(dim(mask) == dim(data_array)[lon_dim])
#      if (length(found_lon_dim) < 0) {
#        stop("The dimension sizes of the provided 'mask' do not match ",
#             "the spatial dimension sizes of the array to interpolate.")
#      } else {
#        names(dim(mask)[found_lon_dim]) <- lon_dim
#      }
#      found_lat_dim <- which(dim(mask) == dim(data_array)[lat_dim])
#      if (length(found_lat_dim) < 0) {
#        stop("The dimension sizes of the provided 'mask' do not match ",
#             "the spatial dimension sizes of the array to interpolate.")
#      } else {
#        names(dim(mask)[found_lat_dim]) <- lat_dim
#      }
#    }
#    lon_position <- which(names(dim(data_array)) == lon_dim)
#    lat_position <- which(names(dim(data_array)) == lat_dim)
#    if (lon_position > lat_position) {
#      if (names(dim(mask))[1] == lon_dim) {
#        mask <- t(mask)
#      }
#    } else {
#      if (names(dim(mask))[1] == lat_dim) {
#        mask <- t(mask)
#      }
#    }
#    ## TODO: Apply mask!!! Preserve attributes
#  }
  # Check if interpolation can be skipped.
  interpolation_needed <- TRUE
  if (!force_remap && !(grid_type == 'custom')) {
    if (length(lons) == grid_lons && length(lats) == grid_lats) {
      if (grid_type == 'regular') {
        if (.isRegularVector(lons) && .isRegularVector(lats)) {
          interpolation_needed <- FALSE
        }
      } else if (grid_type == 'gaussian') {
        # TODO: improve this check. Gaussian quadrature should be used.
        if (.isRegularVector(lons) && !.isRegularVector(lats)) {
          interpolation_needed <- FALSE
        }
      }
    }
  }
  found_lons <- lons
  found_lats <- lats
  if (interpolation_needed) {
    if (nchar(Sys.which('cdo')[1]) < 1) {
      stop("CDO must be installed in order to use the .CDORemap.")
    }
    cdo_version <- 
      strsplit(suppressWarnings(system2("cdo", args = '-V', stderr = TRUE))[[1]], ' ')[[1]][5]
    .warning(paste0("CDORemap: Using CDO version ", cdo_version, "."))
    cdo_version <- as.numeric_version(unlist(strsplit(cdo_version, "[A-Za-z]", fixed = FALSE))[[1]])
    if ((cdo_version >= as.numeric_version('1.7.0')) && (method == 'con')) {
      method <- 'ycon'
    }

    # CDO takes arrays of 3 dimensions or 4 if one of them is unlimited.
    # The unlimited dimension can only be the left-most (right-most in R).
    # There are no restrictions for the dimension names or variable names.
    # The longitude and latitude are detected by their units.
    # There are no restrictions for the order of the limited dimensions.
    # The longitude/latitude variables and dimensions must have the same name.
    # The procedure consists in:
    # - take out the array metadata
    # - be aware of var dimension (replacing the dimension names would do).
    # - take arrays of 4 dimensions always if possible
    # - make the last dimension unlimited when saving to netcdf
    # - if the last dimension is lon or lat, either reorder the array and 
    #   then reorder back or iterate over the dimensions at the right
    #   side of lon AND lat.
    # If the input array has more than 4 dimensions, it is needed to
    # run CDO on each sub-array of 4 dimensions because it can handle
    # only up to 4 dimensions. The shortest dimensions are chosen to 
    # iterate over.
    is_irregular <- FALSE
    if (length(dim(lats)) > 1 && length(dim(lons)) > 1) {
      is_irregular <- TRUE
    }
    attribute_backup <- attributes(data_array)
    other_dims <- which(!(names(dim(data_array)) %in% c(lon_dim, lat_dim)))
    permutation <- NULL
    unlimited_dim <- NULL
    dims_to_iterate <- NULL
    total_slices <- 1
    # Explanation for below: 4 (the maximum accepted by CDO) - 2 (lon, lat) = 2
    other_dims_per_chunk <- ifelse(is_irregular, 1, 2)
    if (length(other_dims) > 1 || (length(other_dims) > 0 && (is_irregular))) {
      # If lat/lon is the last dimension OR the largest other_dims is not the last one, 
      # reorder the largest other dimension to the last as unlimited dim.
      if (!(length(dim(data_array)) %in% other_dims) |
          which.max(dim(data_array)[other_dims]) != length(other_dims)) {
        if (avoid_writes || is_irregular) {
          dims_mod <- dim(data_array)
          dims_mod[which(names(dim(data_array)) %in%
                   c(lon_dim, lat_dim))] <- 0
          dim_to_move <- which.max(dims_mod)
          permutation <- seq_along(dim(data_array))[-dim_to_move]
          permutation <- c(permutation, dim_to_move)
          permutation_back <- sort(permutation, index.return = TRUE)$ix
#          dim_backup <- dim(data_array)
          data_array <- aperm(data_array, permutation)
#          dim(data_array) <- dim_backup[permutation]
          other_dims <- which(!(names(dim(data_array)) %in% c(lon_dim, lat_dim)))
        } else {
          # We allow only lon, lat and 1 more dimension per chunk, so 
          # CDO has no restrictions in the order.
          other_dims_per_chunk <- 1
        }
      }
      other_dims_ordered_by_size <- other_dims[sort(dim(data_array)[other_dims],
                                                    index.return = TRUE)$ix]
      dims_to_iterate <- sort(head(other_dims_ordered_by_size, length(other_dims) -
                                   other_dims_per_chunk))
      if (length(dims_to_iterate) == 0) {
        dims_to_iterate <- NULL
      } else {
        slices_to_iterate <- array(1:prod(dim(data_array)[dims_to_iterate]), 
                                    dim(data_array)[dims_to_iterate])
        total_slices <- prod(dim(slices_to_iterate))
      }
      if ((other_dims_per_chunk > 1) || (other_dims_per_chunk > 0 && is_irregular)) {
        #NOTE: Why don't we use the second line here? In history, that line was never used.
        #      The first line sort() can cause problems. If the largest other_dims is always
        #      the last dim, tail(other_dims) is enough.
        unlimited_dim <- tail(sort(tail(other_dims_ordered_by_size, other_dims_per_chunk)), 1)
        #unlimited_dim <- tail(other_dims)
      }
    }

    result_array <- NULL
    lon_pos <- which(names(dim(data_array)) == lon_dim)
    lat_pos <- which(names(dim(data_array)) == lat_dim)
    dim_backup <- dim(data_array)
    attributes(data_array) <- NULL
    dim(data_array) <- dim_backup
    names(dim(data_array)) <- paste0('dim', seq_along(dim(data_array)))
    names(dim(data_array))[c(lon_pos, lat_pos)] <- c(lon_dim, lat_dim)
    if (!is.null(unlimited_dim)) {
      # This will make ArrayToNc create this dim as unlimited.
      names(dim(data_array))[unlimited_dim] <- 'time'
      # create time variable. The value is random since CDORemap() doesn't support 
      #time remapping now and we just want to avoid cdo warning
      time_attr <- array(seq_len(dim(data_array)[unlimited_dim]), 
                         dim = c(dim(data_array)[unlimited_dim]))
    }
    if (length(dim(lons)) == 1) {
      names(dim(lons)) <- lon_dim
    }
    if (length(dim(lats)) == 1) {
      names(dim(lats)) <- lat_dim
    }
    if (length(dim(lons)) > 1) {
      lon_var_name <- paste0(lon_dim, '_var')
    } else {
      lon_var_name <- lon_dim
    }
    if (length(dim(lats)) > 1) {
      lat_var_name <- paste0(lat_dim, '_var')
    } else {
      lat_var_name <- lat_dim
    }
    if (is_irregular) {
      metadata <- list(list(coordinates = paste(lon_var_name, lat_var_name)))
      names(metadata) <- 'var'
      attr(data_array, 'variables') <- metadata
    }
    names(attr(lons, 'variables')) <- lon_var_name
    names(attr(lats, 'variables')) <- lat_var_name
    if (!is.null(attr(lons, 'variables')[[1]][['dim']])) {
      attr(lons, 'variables')[[1]][['dim']] <- NULL
    }
    if (!is.null(attr(lats, 'variables')[[1]][['dim']])) {
      attr(lats, 'variables')[[1]][['dim']] <- NULL
    }
    lons_lats_taken <- FALSE
    for (i in 1:total_slices) {
      tmp_file <- tempfile('R_CDORemap_', write_dir, fileext = '.nc')
      tmp_file2 <- tempfile('R_CDORemap_', write_dir, fileext = '.nc')
      if (!is.null(dims_to_iterate)) {
        slice_indices <- which(slices_to_iterate == i, arr.ind = TRUE)
        subset <- Subset(data_array, dims_to_iterate, as.list(slice_indices), drop = 'selected')
        # Fix issue 259, curvilinear grid, the order of the dimensions in slices and 
        # coordinates needs to match
        if (is_irregular) {
          pos_lon <- which(names(dim(subset)) == lon_dim)
          pos_lat <- which(names(dim(subset)) == lat_dim)
          pos_lon_dim_in_lons <- which(names(dim(lons)) == lon_dim)
          pos_lat_dim_in_lons <- which(names(dim(lons)) == lat_dim)
        if ((pos_lon > pos_lat && pos_lon_dim_in_lons < pos_lat_dim_in_lons) ||
         (pos_lon < pos_lat && pos_lon_dim_in_lons > pos_lat_dim_in_lons)) {
            new_pos <- seq_along(dim(subset))
            new_pos[pos_lon] <- pos_lat
            new_pos[pos_lat] <- pos_lon
            subset <- .aperm2(subset, new_pos)
          }
          # The unlimited dimension should be placed in the last position
          if ('time' %in% names(dim(subset)) &&
            which(names(dim(subset)) == 'time') != length(dim(subset))) {
            new_pos <- 2:length(dim(subset))
            new_pos[length(dim(subset))] <- 1
            subset <- .aperm2(subset, new_pos)
          }
        }
#        dims_before_crop <- dim(subset)
        # Make sure subset goes along with metadata
        if (is.null(unlimited_dim)) {
          easyNCDF::ArrayToNc(setNames(list(subset, lons, lats), 
                              c('var', lon_var_name, lat_var_name)), tmp_file)
        } else {
        easyNCDF::ArrayToNc(setNames(list(subset, lons, lats, time_attr),
                            c('var', lon_var_name, lat_var_name, 'time')), tmp_file)
        }
      } else {
         if (is_irregular) {
           pos_lon <- which(names(dim(data_array)) == lon_dim)
           pos_lat <- which(names(dim(data_array)) == lat_dim)
           pos_lon_dim_in_lons <- which(names(dim(lons)) == lon_dim)
           pos_lat_dim_in_lons <- which(names(dim(lons)) == lat_dim)
           if ((pos_lon > pos_lat && pos_lon_dim_in_lons < pos_lat_dim_in_lons) ||
         (pos_lon < pos_lat && pos_lon_dim_in_lons > pos_lat_dim_in_lons)) {
             new_pos <- seq_along(dim(data_array))
             new_pos[pos_lon] <- pos_lat
             new_pos[pos_lat] <- pos_lon
             data_array <- .aperm2(data_array, new_pos)
           }
         }
#        dims_before_crop <- dim(data_array)
        if (is.null(unlimited_dim)) {
          easyNCDF::ArrayToNc(setNames(list(data_array, lons, lats),
                              c('var', lon_var_name, lat_var_name)), tmp_file)
        } else {
          easyNCDF::ArrayToNc(setNames(list(data_array, lons, lats, time_attr),
                              c('var', lon_var_name, lat_var_name, 'time')), tmp_file)
        }
      }
      sellonlatbox <- ''
      if (crop) {
        sellonlatbox <- paste0('sellonlatbox,', format(lon_extremes[1], scientific = FALSE), 
                                           ',', format(lon_extremes[2], scientific = FALSE), 
                                           ',', format(lat_extremes[1], scientific = FALSE), 
                                           ',', format(lat_extremes[2], scientific = FALSE), ' -')
      }
      if (is.null(ncores)) {
        err <- try({
          system(paste0("cdo -s ", sellonlatbox, "remap", method, ",", grid, " ", 
                        tmp_file, " ", tmp_file2),
                 ignore.stdout = !print_sys_msg, ignore.stderr = !print_sys_msg)
        })
      } else {
        err <- try({
          system(paste0("cdo -P ", ncores, " -s ", sellonlatbox, "remap", method, ",", 
                        grid, " ", tmp_file, " ", tmp_file2),
                 ignore.stdout = !print_sys_msg, ignore.stderr = !print_sys_msg)
        })
      }
      file.remove(tmp_file)
      if (is(err, 'try-error') || err > 0) {
        stop("CDO remap failed. Set 'print_sys_msg' to TRUE to see CDO system message..")
      }
      ncdf_remapped <- nc_open(tmp_file2)
      if (!lons_lats_taken) {
        found_dim_names <- sapply(ncdf_remapped$var$var$dim, '[[', 'name')
        found_lon_dim <- found_dim_names[which(found_dim_names %in% .KnownLonNames())[1]]
        found_lat_dim <- found_dim_names[which(found_dim_names %in% .KnownLatNames())[1]]
        found_lon_dim_size <- length(ncdf_remapped$dim[[found_lon_dim]]$vals)
        found_lat_dim_size <- length(ncdf_remapped$dim[[found_lat_dim]]$vals)
        found_var_names <- names(ncdf_remapped$var)
        found_lon_var_name <- which(found_var_names %in% .KnownLonNames())
        found_lat_var_name <- which(found_var_names %in% .KnownLatNames())
        if (length(found_lon_var_name) > 0) {
          found_lon_var_name <- found_var_names[found_lon_var_name[1]]
        } else {
          found_lon_var_name <- NULL
        }
        if (length(found_lat_var_name) > 0) {
          found_lat_var_name <- found_var_names[found_lat_var_name[1]]
        } else {
          found_lat_var_name <- NULL
        }
        if (length(found_lon_var_name) > 0) {
          found_lons <- ncvar_get(ncdf_remapped, found_lon_var_name, 
                                  collapse_degen = FALSE)
        } else {
          found_lons <- ncdf_remapped$dim[[found_lon_dim]]$vals
          dim(found_lons) <- found_lon_dim_size
        }
        if (length(found_lat_var_name) > 0) {
          found_lats <- ncvar_get(ncdf_remapped, found_lat_var_name, 
                                  collapse_degen = FALSE)
        } else {
          found_lats <- ncdf_remapped$dim[[found_lat_dim]]$vals
          dim(found_lats) <- found_lat_dim_size
        }
        if (length(dim(lons)) == length(dim(found_lons))) {
          new_lon_name <- lon_dim
        } else {
          new_lon_name <- found_lon_dim
        }
        if (length(dim(lats)) == length(dim(found_lats))) {
          new_lat_name <- lat_dim
        } else {
          new_lat_name <- found_lat_dim
        }
        if (length(dim(found_lons)) > 1) {
          if (which(sapply(ncdf_remapped$var$lon$dim, '[[', 'name') == found_lon_dim) < 
              which(sapply(ncdf_remapped$var$lon$dim, '[[', 'name') == found_lat_dim)) {
            names(dim(found_lons)) <- c(new_lon_name, new_lat_name)
          } else {
            names(dim(found_lons)) <- c(new_lat_name, new_lon_name)
          }
        } else {
          names(dim(found_lons)) <- new_lon_name
        }
        if (length(dim(found_lats)) > 1) {
          if (which(sapply(ncdf_remapped$var$lat$dim, '[[', 'name') == found_lon_dim) < 
              which(sapply(ncdf_remapped$var$lat$dim, '[[', 'name') == found_lat_dim)) {
            names(dim(found_lats)) <- c(new_lon_name, new_lat_name)
          } else {
            names(dim(found_lats)) <- c(new_lat_name, new_lon_name)
          }
        } else {
          names(dim(found_lats)) <- new_lat_name
        }
        lons_lats_taken <- TRUE
      }
      if (!is.null(dims_to_iterate)) {
        if (is.null(result_array)) {
          if (return_array) {
            new_dims <- dim(data_array)
            new_dims[c(lon_dim, lat_dim)] <- c(found_lon_dim_size, found_lat_dim_size)
            lon_pos <- which(names(new_dims) == lon_dim)
            lat_pos <- which(names(new_dims) == lat_dim)
            # Fix issue 259, expected order from CDO output is lon lat
            # If is irregular, lat and lon position need to be checked:
            if (is_irregular && lon_pos > lat_pos) {
              new_pos <- seq_along(new_dims)
              new_pos[lon_pos] <- lat_pos
              new_pos[lat_pos] <- lon_pos
              new_dims <- new_dims[new_pos]
            }
            result_array <- array(dim = new_dims)
            store_indices <- as.list(rep(TRUE, length(dim(result_array))))
          }
        }
        if (return_array) {
          store_indices[dims_to_iterate] <- as.list(slice_indices)
          # If is irregular, the order of dimenesions in result_array and file may be
          # different and need to be checked before reading the temporal file:
          if (is_irregular) {
            test_dims <- dim(ncvar_get(ncdf_remapped, 'var',
                             collapse_degen = FALSE))
            test_dims <- test_dims[which(test_dims > 1)]
            pos_test_dims <- match(names(dim(result_array)),
                                   names(test_dims))
            if (is.unsorted(pos_test_dims, na.rm = TRUE)) {
               # pos_new_dims is used later in the code. Don't overwrite
               pos_new_dims <- seq_along(dim(result_array))
               pos_new_dims[which(!is.na(pos_test_dims))] <-
                 match(names(test_dims), names(dim(result_array)))
               backup_result_array_dims <- dim(result_array)
               dim(result_array) <- dim(result_array)[pos_new_dims]
            }
          }
          result_array <- do.call('[<-', c(list(x = result_array), store_indices, 
                                           list(value = ncvar_get(ncdf_remapped, 'var', 
                                                                  collapse_degen = FALSE))))
        }
      } else {
        new_dims <- dim(data_array)
        new_dims[c(lon_dim, lat_dim)] <- c(found_lon_dim_size, found_lat_dim_size)
        if (is_irregular) {
          lon_pos <- which(names(new_dims) == lon_dim)
          lat_pos <- which(names(new_dims) == lat_dim)
          if (lon_pos > lat_pos) {
            new_pos <- seq_along(new_dims)
            new_pos[lon_pos] <- lat_pos
            new_pos[lat_pos] <- lon_pos
            new_dims <- new_dims[new_pos]
          }
        }
        result_array <- ncvar_get(ncdf_remapped, 'var', collapse_degen = FALSE)
        dim(result_array) <- new_dims
      }
      nc_close(ncdf_remapped)
      file.remove(tmp_file2)
    }
    # If is irregular, the order of dimension may need to be recovered after reading all the file:
    if (is_irregular & (!is.null(dims_to_iterate))) {
      if (exists('pos_new_dims')) {
        pos_new_dims <- seq_along(dim(result_array))
        dims_to_change <- match(names(backup_result_array_dims),
                                names(dim(result_array)))
        pos_new_dims[which(dims_to_change != 1)] <-
          dims_to_change[which(dims_to_change != 1)]
        result_array <- .aperm2(result_array, pos_new_dims)
      }
    }

    if (!is.null(permutation)) {
      dim_backup <- dim(result_array)
      result_array <- aperm(result_array, permutation_back)
      dim(result_array) <- dim_backup[permutation_back]
    }
    # Now restore the metadata
    result_is_irregular <- FALSE
    if (length(dim(found_lats)) > 1 && length(dim(found_lons)) > 1) {
      result_is_irregular <- TRUE
    }
    attribute_backup[['dim']][which(names(dim(result_array)) == lon_dim)] <- 
      dim(result_array)[lon_dim]
    attribute_backup[['dim']][which(names(dim(result_array)) == lat_dim)] <- 
      dim(result_array)[lat_dim]
    names(attribute_backup[['dim']])[which(names(dim(result_array)) == lon_dim)] <- new_lon_name
    names(attribute_backup[['dim']])[which(names(dim(result_array)) == lat_dim)] <- new_lat_name
    if (!is.null(attribute_backup[['variables']]) && 
        (length(attribute_backup[['variables']]) > 0)) {
      for (var in seq_along(attribute_backup[['variables']])) {
        if (length(attribute_backup[['variables']][[var]][['dim']]) > 0) {
          for (dim in seq_along(attribute_backup[['variables']][[var]][['dim']])) {
            dim_name <- NULL
            if ('name' %in% names(attribute_backup[['variables']][[var]][['dim']][[dim]])) {
              dim_name <- attribute_backup[['variables']][[var]][['dim']][[dim]][['name']]
              if (dim_name %in% c(lon_dim, lat_dim)) {
                if (dim_name == lon_dim) {
                  attribute_backup[['variables']][[var]][['dim']][[dim]][['name']] <- new_lon_name
                } else {
                  attribute_backup[['variables']][[var]][['dim']][[dim]][['name']] <- new_lat_name
                }
              }
            } else if (!is.null(names(attribute_backup[['variables']][[var]][['dim']]))) {
              dim_name <- names(attribute_backup[['variables']][[var]][['dim']])[dim]
              if (dim_name %in% c(lon_dim, lat_dim)) {
                if (dim_name == lon_dim) {
                  tmp <- which(names(attribute_backup[['variables']][[var]][['dim']]) == lon_dim)
                  names(attribute_backup[['variables']][[var]][['dim']])[tmp] <- new_lon_name
                } else {
                  tmp <- which(names(attribute_backup[['variables']][[var]][['dim']]) == lat_dim)
                  names(attribute_backup[['variables']][[var]][['dim']])[tmp] <- new_lat_name
                }
              }
            }
            if (!is.null(dim_name)) {
              if (dim_name %in% c(lon_dim, lat_dim)) {
                if (dim_name == lon_dim) {
                  new_vals <- found_lons[TRUE]
                } else if (dim_name == lat_dim) {
                  new_vals <- found_lats[TRUE]
                }
                if (!is.null(attribute_backup[['variables']][[var]][['dim']][[dim]][['len']])) {
                  attribute_backup[['variables']][[var]][['dim']][[dim]][['len']] <- 
                    length(new_vals)
                }
                if (!is.null(attribute_backup[['variables']][[var]][['dim']][[dim]][['vals']])) {
                  if (!result_is_irregular) {
                    attribute_backup[['variables']][[var]][['dim']][[dim]][['vals']] <- new_vals
                  } else {
                    attribute_backup[['variables']][[var]][['dim']][[dim]][['vals']] <- 
                      seq_along(new_vals)
                  }
                }
              }
            }
          }
        }
        if (!is_irregular && result_is_irregular) {
          attribute_backup[['coordinates']] <- paste(lon_var_name, lat_var_name)
        } else if (is_irregular && !result_is_irregular) {
          attribute_backup[['coordinates']] <- NULL
        }
      }
    }
    attributes(result_array) <- attribute_backup
    lons_attr_bk[['dim']] <- dim(found_lons)
    if (!is.null(lons_attr_bk[['variables']]) && (length(lons_attr_bk[['variables']]) > 0)) {
      for (var in seq_along(lons_attr_bk[['variables']])) {
        if (length(lons_attr_bk[['variables']][[var]][['dim']]) > 0) {
          dims_to_remove <- NULL
          for (dim in seq_along(lons_attr_bk[['variables']][[var]][['dim']])) {
            dim_name <- NULL
            if ('name' %in% names(lons_attr_bk[['variables']][[var]][['dim']][[dim]])) {
              dim_name <- lons_attr_bk[['variables']][[var]][['dim']][[dim]][['name']]
              if (dim_name %in% c(lon_dim, lat_dim)) {
                if (dim_name == lon_dim) {
                  lons_attr_bk[['variables']][[var]][['dim']][[dim]][['name']] <- new_lon_name
                } else {
                  lons_attr_bk[['variables']][[var]][['dim']][[dim]][['name']] <- new_lat_name
                }
              }
            } else if (!is.null(names(lons_attr_bk[['variables']][[var]][['dim']]))) {
              dim_name <- names(lons_attr_bk[['variables']][[var]][['dim']])[dim]
              if (dim_name %in% c(lon_dim, lat_dim)) {
                if (dim_name == lon_dim) {
                  tmp <- which(names(lons_attr_bk[['variables']][[var]][['dim']]) == lon_dim)
                  names(lons_attr_bk[['variables']][[var]][['dim']])[tmp] <- new_lon_name
                } else {
                  tmp <- which(names(lons_attr_bk[['variables']][[var]][['dim']]) == lat_dim)
                  names(lons_attr_bk[['variables']][[var]][['dim']])[tmp] <- new_lat_name
                }
              }
            }
            if (!is.null(dim_name)) {
              if (dim_name %in% c(lon_dim, lat_dim)) {
                if (dim_name == lon_dim) {
                  new_vals <- found_lons[TRUE]
                } else if (dim_name == lat_dim) {
                  new_vals <- found_lats[TRUE]
                  if (!result_is_irregular) {
                    dims_to_remove <- c(dims_to_remove, dim)
                  }
                }
                if (!is.null(lons_attr_bk[['variables']][[var]][['dim']][[dim]][['len']])) {
                  lons_attr_bk[['variables']][[var]][['dim']][[dim]][['len']] <- length(new_vals)
                }
                if (!is.null(lons_attr_bk[['variables']][[var]][['dim']][[dim]][['vals']])) {
                  if (!result_is_irregular) {
                    lons_attr_bk[['variables']][[var]][['dim']][[dim]][['vals']] <- new_vals
                  } else {
                    lons_attr_bk[['variables']][[var]][['dim']][[dim]][['vals']] <-
                      seq_along(new_vals)
                  }
                }
              }
            }
          }
          if (length(dims_to_remove) > 1) {
            lons_attr_bk[['variables']][[var]][['dim']] <- 
              lons_attr_bk[['variables']][[var]][['dim']][[-dims_to_remove]]
          }
        }
      }
      names(lons_attr_bk[['variables']])[1] <- lon_var_name
      lons_attr_bk[['variables']][[1]][['units']] <- 'degrees_east'
    }
    attributes(found_lons) <- lons_attr_bk
    lats_attr_bk[['dim']] <- dim(found_lats)
    if (!is.null(lats_attr_bk[['variables']]) && (length(lats_attr_bk[['variables']]) > 0)) {
      for (var in seq_along(lats_attr_bk[['variables']])) {
        if (length(lats_attr_bk[['variables']][[var]][['dim']]) > 0) {
          dims_to_remove <- NULL
          for (dim in seq_along(lats_attr_bk[['variables']][[var]][['dim']])) {
            dim_name <- NULL
            if ('name' %in% names(lats_attr_bk[['variables']][[var]][['dim']][[dim]])) {
              dim_name <- lats_attr_bk[['variables']][[var]][['dim']][[dim]][['name']]
              if (dim_name %in% c(lon_dim, lat_dim)) {
                if (dim_name == lon_dim) {
                  lons_attr_bk[['variables']][[var]][['dim']][[dim]][['name']] <- new_lon_name
                } else {
                  lons_attr_bk[['variables']][[var]][['dim']][[dim]][['name']] <- new_lat_name
                }
              }
            } else if (!is.null(names(lats_attr_bk[['variables']][[var]][['dim']]))) {
              dim_name <- names(lats_attr_bk[['variables']][[var]][['dim']])[dim]
              if (dim_name %in% c(lon_dim, lat_dim)) {
                if (dim_name == lon_dim) {
                  tmp <- which(names(lats_attr_bk[['variables']][[var]][['dim']]) == lon_dim)
                  names(lats_attr_bk[['variables']][[var]][['dim']])[tmp] <- new_lon_name
                } else {
                  tmp <- which(names(lats_attr_bk[['variables']][[var]][['dim']]) == lat_dim)
                  names(lats_attr_bk[['variables']][[var]][['dim']])[tmp] <- new_lat_name
                }
              }
            }
            if (!is.null(dim_name)) {
              if (dim_name %in% c(lon_dim, lat_dim)) {
                if (dim_name == lon_dim) {
                  new_vals <- found_lons[TRUE]
                  if (!result_is_irregular) {
                    dims_to_remove <- c(dims_to_remove, dim)
                  }
                } else if (dim_name == lat_dim) {
                  new_vals <- found_lats[TRUE]
                }
                if (!is.null(lats_attr_bk[['variables']][[var]][['dim']][[dim]][['len']])) {
                  lats_attr_bk[['variables']][[var]][['dim']][[dim]][['len']] <- length(new_vals)
                }
                if (!is.null(lats_attr_bk[['variables']][[var]][['dim']][[dim]][['vals']])) {
                  if (!result_is_irregular) {
                    lats_attr_bk[['variables']][[var]][['dim']][[dim]][['vals']] <- new_vals
                  } else {
                    lats_attr_bk[['variables']][[var]][['dim']][[dim]][['vals']] <- 
                      seq_along(new_vals)
                  }
                }
              }
            }
          }
          if (length(dims_to_remove) > 1) {
            lats_attr_bk[['variables']][[var]][['dim']] <-
              lats_attr_bk[['variables']][[var]][['dim']][[-dims_to_remove]]
          }
        }
      }
      names(lats_attr_bk[['variables']])[1] <- lat_var_name
      lats_attr_bk[['variables']][[1]][['units']] <- 'degrees_north'
    }
    attributes(found_lats) <- lats_attr_bk
  }
  list(data_array = if (return_array) {
                      if (interpolation_needed) {
                        result_array
                      } else {
                        data_array
                      }
                    } else {
                      NULL
                    },
       lons = found_lons, lats = found_lats)
}

