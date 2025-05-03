#'CDO Remap Data Transformation for 'startR'
#'
#'This is a transform function that uses CDO software to remap longitude-latitude 
#'data subsets onto a specified target grid, intended for use as parameter 
#''transform' in a Start() call. This function complies with the input/output 
#'interface required by Start() defined in the documentation for the parameter 
#''transform' of function Start().\cr\cr
#'This function uses the function CDORemap() in the package 's2dv' to 
#'perform the interpolation, hence CDO is required to be installed.
#'
#'@param data_array A data array to be transformed. See details in the 
#'  documentation of the parameter 'transform' of the function Start().
#'@param variables A list of auxiliary variables required for the transformation, 
#'  automatically provided by Start(). See details in the documentation of the 
#'  parameter 'transform' of the function Start().
#'@param file_selectors A charcter vector indicating the information of the path of
#'  the file parameter 'data_array' comes from. See details in the documentation of
#'  the parameter 'transform' of the function Start(). The default value is NULL.
#'@param crop_domain A list of the transformed domain of each transform 
#'  variable, automatically provided by Start().
#'@param \dots A list of additional parameters to adjust the transform process, 
#'  as provided in the parameter 'transform_params' in a Start() call. See details
#'  in the documentation of the parameter 'transform' of the function Start().
#'
#'@return An array with the same amount of dimensions as the input data array, 
#'  potentially with different sizes, and potentially with the attribute 
#'  'variables' with additional auxiliary data. See details in the documentation 
#'  of the parameter 'transform' of the function Start().
#'@seealso \code{\link[s2dv]{CDORemap}}
#'
#'@examples
#'# Used in Start():
#'  data_path <- system.file('extdata', package = 'startR')
#'  path_obs <- file.path(data_path, 'obs/monthly_mean/$var$/$var$_$sdate$.nc')
#'  sdates <- c('200011')
#'  \dontrun{
#'  data <- Start(dat = list(list(path = path_obs)),
#'                var = 'tos',
#'                sdate = sdates,
#'                time = 'all',
#'                latitude = values(list(-60, 60)),
#'                latitude_reorder = Sort(decreasing = TRUE),
#'                longitude = values(list(-120, 120)),
#'                longitude_reorder = CircularSort(-180, 180),
#'                transform = CDORemapper,
#'                transform_params = list(grid = 'r360x181',
#'                                        method = 'conservative'),
#'                transform_vars = c('latitude', 'longitude'),
#'                return_vars = list(latitude = 'dat',
#'                                   longitude = 'dat',
#'                                   time = 'sdate'),
#'                retrieve = FALSE)
#' }
#'@importFrom s2dv CDORemap
#'@importFrom utils getFromNamespace
#'@export
CDORemapper <- function(data_array, variables, file_selectors = NULL, 
                        crop_domain = NULL, ...) {
  file_dims <- names(file_selectors)
  known_lon_names <- getFromNamespace('.KnownLonNames', 'startR')()
  known_lat_names <- getFromNamespace('.KnownLatNames', 'startR')()
  if (!any(known_lon_names %in% names(variables)) ||
      !any(known_lat_names %in% names(variables))) {
    stop("The longitude and latitude variables must be requested in ",
         "'return_vars' and specified in 'transform_vars' for the ",
         "CDORemapper to work.")
  }
  lon_name <- names(variables)[which(names(variables) %in% known_lon_names)[1]]
  lons <- variables[[lon_name]]
  if (!is.null(dim(lons))) {
    dims_to_subset <- which(names(dim(lons)) %in% file_dims)
    if (length(dims_to_subset) > 0) {
      lons_to_use <- as.list(rep(TRUE, length(dim(lons))))
      names(lons_to_use) <- names(dim(lons))
      lons_to_use[dims_to_subset] <- as.list(rep(1, length(dims_to_subset)))
      attr_bk <- attributes(lons)
      lons <- do.call('[', c(list(x = lons), lons_to_use, list(drop = TRUE)))
      attributes(lons) <- attr_bk
    }
  }
  lat_name <- names(variables)[which(names(variables) %in% known_lat_names)[1]]
  lats <- variables[[lat_name]]
  if (!is.null(dim(lats))) {
    dims_to_subset <- which(names(dim(lats)) %in% file_dims)
    if (length(dims_to_subset) > 0) {
      lats_to_use <- as.list(rep(TRUE, length(dim(lats))))
      names(lats_to_use) <- names(dim(lats))
      lats_to_use[dims_to_subset] <- as.list(rep(1, length(dims_to_subset)))
      attr_bk <- attributes(lons)
      lats <- do.call('[', c(list(x = lats), lats_to_use, list(drop = TRUE)))
      attributes(lats) <- attr_bk
    }
  }
  extra_params <- list(...)

  if (!all(c('grid', 'method') %in% names(extra_params))) {
    stop("Parameters 'grid' and 'method' must be specified for the ",
         "CDORemapper, via the 'transform_params' argument.")
  }

  # Use crop_domain to get 'crop'
  if (!is.null(crop_domain)) {
    ## lon
    lon_name <- names(crop_domain)[which(names(crop_domain) %in% known_lon_names)]
    crop_lon <- unlist(crop_domain[[lon_name]])
    ## lat
    lat_name <- names(crop_domain)[which(names(crop_domain) %in% known_lat_names)]
    crop_lat <- unlist(crop_domain[[lat_name]])
    crop_values <- c(crop_lon, crop_lat)

    if ('crop' %in% names(extra_params)) {
      warning("Argument 'crop' in 'transform_params' for CDORemapper() is ",
               "deprecated. It is automatically assigned as the selected domain ",
               "in Start() call.")
    }
    extra_params[['crop']] <- crop_values
  }

  result <- do.call(s2dv::CDORemap, c(list(data_array, lons, lats), extra_params))
  return_variables <- list(result$lons, result$lats)
  names(return_variables) <- c(lon_name, lat_name)
  list(data_array = result$data_array, variables = return_variables)
}
