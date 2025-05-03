
#' Compare an EURO-CORDEX inventory for different variables
#'
#' Casts the result from \code{\link{get_inventory}} for different variables in
#' order to compare completeness of the inventory. Adds columns for checking
#' equality of years and number of files.
#'
#' @param data_inventory A data.table as resulting from
#'   \code{\link{get_inventory}}.
#' @param vars Character vector of variables to compare. If \code{NULL}, will use
#'   all variables in \code{data_inventory}.
#'
#' @return The casted data.table with boolean columns if all years and number of
#'   files are equal for all variables.
#'
#' @export
#'
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'
#' path <- "/mnt/CEPH_BASEDATA/METEO/SCENARIO"
#' dat <- get_inventory(path)
#' dat_compare <- compare_variables_in_inventory(dat, c("tas","rsds","pr"))
#' }
compare_variables_in_inventory <- function(data_inventory, vars = NULL){

  # NSE in R CMD check
  list_files <- nn_files <- period_contiguous <- date_end <- variable <- NULL
  all_date_start_equal <- all_years_equal <- NULL


  dat <- copy(data_inventory)

  if("list_files" %in% names(dat)) {
    dat[, list_files := NULL]
  }

  if(is.null(vars)) vars <- unique(dat$variable)

  # remove some columns for casting
  dat[, ":="(nn_files = NULL, period_contiguous = NULL, date_end = NULL)]
  dat[variable %in% vars] %>%
    dcast(... ~ variable, value.var = c("date_start", "total_simulation_years")) -> dat_compare

  dat_compare[,
              all_date_start_equal := apply(as.matrix(.SD), 1, function(x) length(unique(x)) == 1),
              .SDcols = patterns("date_start")]
  dat_compare[,
              all_years_equal := apply(as.matrix(.SD), 1, function(x) length(unique(x)) == 1),
              .SDcols = patterns("total_simulation_years")]

  dat_compare

}
