#' CSTools Data Retreival Function
#'
#' This function aggregates, subsets and retrieves sub-seasonal, seasonal, decadal or climate projection data from NetCDF files in a local file system or on remote OPeNDAP servers, and arranges it for easy application of the CSTools functions.
#'
#' It receives any number of parameters (`...`) that are automatically forwarded to the `s2dv::Load` function. See details in `?s2dv::Load`.
#'
#' It is recommended to use this function in combination with the `zeallot::"%<-%"` operator, to directly assing the two returned 's2dv_cube's to two separate variables, which can then be sent independently to other functions in CSTools as needed. E.g.: `c(exp, obs) <- CST_Load(...)`.
#'
#' @param ... Parameters that are automatically forwarded to the `s2dv::Load` function. See details in `?s2dv::Load`.
#' @return A list with one or two S3 objects, named 'exp' and 'obs', of the class 's2dv_cube', containing experimental and date-corresponding observational data, respectively. These 's2dv_cube's can be ingested by other functions in CSTools. If the parameter `exp` in the call to `CST_Load` is set to `NULL`, then only the 'obs' component is returned, and viceversa.
#' @author Nicolau Manubens, \email{nicolau.manubens@bsc.es}
#' @importFrom s2dv Load
#' @importFrom utils glob2rx
#' @export
#' @examples
#' \dontrun{
#' library(zeallot)
#' startDates <- c('20001101', '20011101', '20021101', 
#'                 '20031101', '20041101', '20051101')
#' c(exp, obs) %<-% 
#'   CST_Load(
#'     var = 'tas', 
#'     exp = 'system5c3s', 
#'     obs = 'era5', 
#'     nmember = 15,
#'     sdates = startDates,
#'     leadtimemax = 3,
#'     latmin = 27, latmax = 48,
#'     lonmin = -12, lonmax = 40,
#'     output = 'lonlat',
#'     nprocs = 1
#'   )
#' }
#' \dontshow{
#' exp <- CSTools::lonlat_temp$exp
#' obs <- CSTools::lonlat_temp$obs
#' }
CST_Load <- function(...) {
  exp <- Load(...)
  result <- as.s2dv_cube(exp)
  result
}
