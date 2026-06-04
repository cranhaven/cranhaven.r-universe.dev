#' @title long_to_wide
#'
#' @description Reshape a \code{data.table} from a "long" format to a "wide" format.
#'
#' Function calls \code{data.table::dcast()} to reshape a long single column and
#'   its values to multiple columns.
#'
#' @param dt The required \code{data.table} with a long column format.
#' @param id_v A required vector of column names from 'dt' that act as identifiers and
#'   are not part of the widened column.
#' @param parameter_col A column name from 'dt' whose unique values will become
#'   column names for the new expanded \code{data.table}.
#' @param value_col A required column name or vector of column names from 'dt' whose values will fall under
#'   the new expanded \code{data.table}.
#'
#' @return A reshaped \code{data.table} in the "wide" format.
#'
#' @importFrom data.table copy
#' @importFrom data.table dcast
#' @importFrom data.table setnames
#'
#' @examples
#' \dontrun{
#'   # Requires Census Bureau API key
#'   library(data.table)
#'   library(httr2)
#'   library(jsonlite)
#'   library(stringr)
#'   library(RcensusPkg)
#'
#'   # Request for data from Census Bureau in "long" form
#'   B19001_1yr_long_dt <- RcensusPkg::get_vintage_data(
#'     dataset = "acs/acs1",
#'     vintage = 2016,
#'     group = "B19001",
#'     region = "state",
#'     wide_to_long = TRUE
#'   )
#'
#'   # Resulting data.table is in the "long" form. Convert it back to
#'   # to the wide form.
#'   B19001_1yr_wide_dt <- RcensusPkg::long_to_wide(
#'     dt = B19001_1yr_long_dt,
#'     parameter_col = "variable",
#'     value_col = c("estimate", "moe")
#'   )
#' }
#' @export
long_to_wide <- function(
  dt = NULL,
  id_v = c("NAME", "GEOID"),
  parameter_col = NULL,
  value_col = NULL
){
  # Check arguments
  if(is.null(dt)){
    stop("The 'dt' argument is required.")
  }
  if(is.null(parameter_col)){
    stop("The 'parameter_col' argument is required.")
  }
  if(is.null(value_col)){
    stop("The 'value_col' argument is required.")
  }

  dt_copy <- data.table::copy(dt)
  #col_names_dt <- data.table::data.table(names = names(dt_copy))

  # Create the formula for dcast
  a_formula_str <- paste(id_v, collapse = "+")
  a_formula_str <- paste0(a_formula_str, " ~ ", parameter_col)

  dt_wide_dt <- data.table::dcast(
    data = dt,
    formula = stats::as.formula(a_formula_str),
    value.var = value_col
  )

  if("estimate" %in% value_col & "moe" %in% value_col){
    for(name in names(dt_wide_dt)){
      if(startsWith(name, "estimate")){
        data.table::setnames(dt_wide_dt, old = name, new = paste0(substr(name, start = 10, stop = nchar(name)), "E"))
      }else if(startsWith(name, "moe")){
        data.table::setnames(dt_wide_dt, old = name, new = paste0(substr(name, start = 5, stop = nchar(name)), "M"))
      }
    }
  }

  return(dt_wide_dt)
}
