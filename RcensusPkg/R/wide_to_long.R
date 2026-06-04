#' @title wide_to_long
#'
#' @description Reshape a data frame from a "wide" format to a "long" format.
#'
#' Function is a helper in calling data.table's melt() function
#'   to reshape a wide data frame to a long form.
#'
#' @param dt The data frame with a wide collection of column variables.
#'   This parameter is required.
#' @param id_v A character vector of column from \code{dt} that are not to be
#'   consolidated and act as identifier columns for the new long form.
#'   This parameter is required.
#' @param measure_v An optional character vector that sets the column measures from
#'   \code{dt} that are to be consolidated. If not specified then all the columns
#'   not in \code{id_v} are considered measures and will be consolidated.
#' @param variable_name An optional string that sets the column name for
#'   the consolidated column names.
#' @param value_name An optional string that sets the column name for the
#'   consolidated values.
#' @param na_rm An optional logical which if TRUE will remove rows with NA values.
#'
#' @return A reshaped data frame in the "long" format.
#'
#' @importFrom data.table as.data.table melt copy
#'
#' @examples
#' \dontrun{
#'   # Requires Census Bureau API key
#'   library(data.table)
#'   library(downloader)
#'   library(jsonlite)
#'   library(RcensusPkg)
#'   # Request for data from Census Bureau which comes in the "wide" form
#'   B19001_1yr_wide_dt <- RcensusPkg::get_vintage_data(
#'     dataset = "acs/acs1",
#'     vintage = 2016,
#'     group = "B19001",
#'     region = "state"
#'   )
#'   # Convert the returned data.table into "long" form
#'   B19001_1yr_long_dt <- RcensusPkg::wide_to_long(
#'     dt = B19001_1yr_wide_dt,
#'     id_v = c("NAME","GEOID")
#'   )
#' }
#' @export
wide_to_long <- function(
    dt = NULL,
    id_v = c("NAME","GEOID"),
    measure_v = NULL,
    variable_name = "variable",
    value_name = "estimate",
    na_rm = FALSE
){
  dt_copy <- data.table::copy(dt)
  dt_copy_dt <- data.table::as.data.table(dt_copy)

  return_dt <- data.table::melt(
    data = dt_copy_dt,
    id.vars = id_v,
    measure.vars = measure_v,
    variable.name = variable_name,
    value.name = value_name,
    na.rm = na_rm
  )
  return(return_dt)
}
