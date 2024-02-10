#' Get LERI data
#'
#' This function searches for LERI data by date, returning a
#' \code{Raster*} object.
#'
#' The Landscape Evaporative Response Index (LERI) is available from 2000 to
#' present, with a ~5 day lag to the current date. Products are available at
#' multiple timescales. Monthly LERI data are available at the 1, 3, 7, and 12
#' month timescale for each month of the year. During the growing season (April
#' through the end of October), there are two additional LERI products:
#'
#' \itemize{
#'   \item "Non-accumulated" 8-day data, which represent deviations in LERI
#'     within an eight day time frame (for example, the time period from
#'     May 01 to May 08).
#'   \item "Accumulated" 8-day data, which represent deviations in LERI for
#'     the period from April 1 to the end of the eight day time frame (for
#'     example, the time period from April 01 to May 08).
#' }
#'
#' The values in LERI rasters are percentiles for estimates of actual
#' evapotranspiration computed by the operational Simplified Surface Energy
#' Balance (SEEBop) model, which uses remotely sensed MODIS thermal imagery
#' with climatological reference evapotranspiration as described in Senay et
#' al., 2013.
#'
#' More information about how the monthly, non-accumulated 8-day, and
#' accumulated 8-day data differ can be found at
#' \url{https://www.esrl.noaa.gov/psd/leri/}.
#'
#' @param date An object of class Date or a character string formatted as
#' %Y-%m-%d (e.g., "2016-04-01") which specifies the date(s) for which data
#' are to be acquired. To specify a time interval or date range, date can be
#' a vector of class Date such as produced by \code{seq.Date}.
#' @param product A string that specifies which LERI product to get, e.g.,
#' "1 month", "3 month", "7 month", or "12 month" for their monthly data, or
#' "8 day ac", or "8 day nac" for accumulated and non-accumulated
#' (respectively) 8-day LERI products. Fractional timescales
#' are not supported, and will be rounded to the nearest integer (e.g., "1.1
#' month" will be converted to "1 month").
#' @param dir Directory to for downloaded LERI data. By default this will be
#' a temporary directory. This should be a file path specified as a string.
#' @param overwrite Boolean to indicate whether to overwrite LERI data that
#' already exist locally in \code{dir}. Defaults to FALSE.
#' @return A Raster* object containing LERI data. Each layer in this object
#' corresponds to a unique LERI data file. In cases where one LERI file covers
#' multiple dates that were provided in the \code{date} argument, the number
#' of layers in the returned Raster* object may be less than the number of
#' dates queried. For example, if a user requests the 8 day LERI product for
#' two consecutive dates, both of those dates might be contained in the same
#' 8 day LERI data product. The time intervals covered by each layer in the
#' Raster* object returned by \code{get_leri} are available as layer names.
#'
#' @examples
#' \donttest{
#' # this may take a while, as it downloads ~72 MB
#' get_leri(date = "2018-01-01", product = "1 month")
#'
#' # multi-day example that returns a two-layer raster
#' r <- get_leri(date = c("2018-08-12", "2018-08-13"), product = "8 day nac")
#' names(r) # has two elements (Aug05-Aug12, and Aug13-Aug20)
#' }
#'
#' @references
#' Senay, Gabriel B., Stefanie Bohms, Ramesh K. Singh, Prasanna H. Gowda,
#' Naga M. Velpuri, Henok Alemu, James P. Verdin, 2013b. Operational
#' Evapotranspiration Mapping Using Remote Sensing and Weather Datasets: A New
#' Parameterization for the SSEB Approach. Journal of the American Water
#' Resources Association (JAWRA). 49(3):577-591.
#' \url{http://onlinelibrary.wiley.com/doi/10.1111/jawr.12057/full}
#'
#' @export
get_leri <- function(date, product, dir = tempdir(), overwrite = FALSE) {
  ncdf4::nc_version() # verifies ncdf4 installation
  parsed_product <- parse_product(product)
  parsed_date <- parse_date(date, parsed_product)
  url <- make_url(parsed_date, parsed_product)
  fnames <- basename(url)
  local_path <- file.path(dir, fnames)
  for (i in seq_along(url)) {
    if (overwrite | !file.exists(local_path[i])) {
      utils::download.file(url[i], local_path[i])
    }
  }
  r <- raster::stack(local_path)
  r <- raster::reclassify(r, cbind(-Inf, 0, NA), right = FALSE)
  r <- raster::reclassify(r, cbind(100, Inf, NA), right = TRUE)
  names(r) <- fnames
  raster::crs(r) <- "+init=epsg:4326"
  r
}


parse_date <- function(date, parsed_product) {
  if (class(date) != "Date") {
    tryCatch(date <- as.Date(date),
             error = function(c) {
               stop(paste("Couldn't coerce date(s) to a Date object.",
                          "Try formatting date(s) as: %Y-%m-%d,",
                          "or use Date objects for the date argument",
                          "(see ?Date)."))
             }
    )
  }
  todays_date <- format(Sys.time(), "%Y-%m-%d")
  if (any(date > todays_date)) {
    stop("All provided dates must be <= the current date.")
  }
  if (any(date <= as.Date('2000-01-01'))) {
    stop("LERI data are not available prior to 2000.")
  }
  month_int <- as.integer(format(date, '%m'))
  month_outside_8d_range <- any(month_int < 4 | month_int > 10)
  if (parsed_product$is_8_day & month_outside_8d_range) {
    stop('8 day products are only available from April 01 to October 31.')
  }
  date
}


parse_product <- function(product) {
  if (length(product) != 1) {
    stop("The product argument must have length one, e.g., '3 month'")
  }
  valid_products <- c('1 month', '3 month', '7 month', '12 month',
                      '8 day ac', '8 day nac')
  if (!(product %in% valid_products)) {
    stop(paste("The product argument must be one of the following:",
               paste(valid_products, collapse = ", ")))
  }
  list(product=product, is_8_day=grepl('8 day', product))
}


find_8d_interval <- function(date, product) {
  stopifnot(length(date) == 1)
  yr <- as.integer(format(date, '%Y'))
  end_dates <- seq(as.Date(paste0(yr, '-04-06')),
                   as.Date(paste0(yr, '-10-31')),
                   by = 8)
  end_date_geq <- end_dates >= date
  end_date <- min(end_dates[end_date_geq])

  # find start date (differs for non-accumulated & accumulated data)
  is_nac <- grepl('nac', product)
  if (is_nac) {
    nac_start_dates <- c(as.Date(paste0(yr, '-04-01')),
                         end_dates[-length(end_dates)] + 1)
    start_date_leq <- nac_start_dates <= date
    start_date <- nac_start_dates[start_date_leq & end_date_geq]
  } else {
    start_date <- as.Date(paste0(yr, '-04-01'))
  }
  dates <- c(start_date, end_date)
  date_string <- paste(format(dates, '%b%d'), collapse = '-')
  date_string
}


make_url <- function(parsed_date, parsed_product) {
  baseurl <- 'ftp://ftp.cdc.noaa.gov/Projects/LERI/CONUS_archive/'
  year <- format(parsed_date, '%Y')
  if (parsed_product$is_8_day) {
    ac_or_nac <- gsub('8 day ', '', parsed_product$product)
    date_interval_string <- vapply(parsed_date, find_8d_interval,
                                   product=parsed_product$product,
                                   FUN.VALUE = 'a string')
    url <- paste0(baseurl, '8day/data/', year, '/',
                  'LERI_8day-', ac_or_nac, '_', date_interval_string,
                  '_', year, '.nc')
  } else {
    month_timescale_int <- as.integer(gsub(' month', '',
                                           parsed_product$product))
    month_timescale_string <- sprintf('%02d', month_timescale_int)
    url <- paste0(baseurl, 'data/', year,
                  '/LERI_', month_timescale_string, 'mn_',
                  year, format(parsed_date, '%m'), '01.nc')
  }
  unique(url)
}
