#' @title get_category_strings
#'
#' @description For a Census Bureau categorical variable return a \code{data.table} with the
#'   variable's integer values and corresponding label strings.
#'
#' The function will attempt to locate a Bureau's categorical variable by name and
#'   return a \code{data.table} with both the integer values and corresponding label strings. If
#'   the variable is not located then NULL is returned.  Definitions for the variables were
#'   obtained from
#'   \href{https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2019.html}{Population Estimates Categorical Variables}
#'   and other Bureau sources.
#' @param name A required string that is the name of the categorical variable of interest.
#' @param get_names An optional logical which if \code{TRUE} the function will return
#'  a vector of categorical variable names recognized by the function.
#' @param start_idx An optional integer that sets the starting index of the variable's integer/label pairs.
#'   If the value is \code{NULL}(the default), then the variable's entire integer/label pairs are returned.
#' @param end_idx An optional integer that sets the ending row index of the returned variable's integer/label pairs.
#'
#' @return A \code{data.table} with the variable's integer values and corresponding label strings.
#'
#' @examples
#' library(data.table)
#' library(RcensusPkg)
#'
#' # Get the names currently supported category variables
#' category_names_v <- RcensusPkg::get_category_strings(get_names = TRUE)
#'
#' # Get the integer/string pairs for the category name "sex"
#' sex_category_dt <- RcensusPkg::get_category_strings(name = "sex")
#'
#' @import data.table
#'
#' @export
get_category_strings <- function(name = NULL, get_names = FALSE, start_idx = NULL, end_idx = NULL){
  categories <- list(
    "sex" = data.table::data.table(
      val = 0:2,
      sex_label = c("Both Sexes", "Male", "Female")
    ),
    "agegroup" = data.table::data.table(
      val = 0:31,
      agegroup_label = c("All ages", "Age 0 to 4 years", "Age 5 to 9 years", "Age 10 to 14 years",
                "Age 15 to 19 years", "Age 20 to 24 years", "Age 25 to 29 years", "Age 30 to 34 years",
                "Age 35 to 39 years", "Age 40 to 44 years", "Age 45 to 49 years", "Age 50 to 54 years",
                "Age 55 to 59 years", "Age 60 to 64 years", "Age 65 to 69 years", "Age 70 to 74 years",
                "Age 75 to 79 years", "Age 80 to 84 years", "Age 85 years and older", "Under 18 years",
                "5 to 13 years", "14 to 17 years", "18 to 64 years", "18 to 24 years",
                "25 to 44 years", "45 to 64 years", "65 years and over", "85 years and over",
                "16 years and over", "18 years and over", "15 to 44 years", "Median age"
      )
    ),
    "race" = data.table::data.table(
      val = 0:11,
      race_label = c("All races", "White alone", "Black alone", "American Indian and Alaska Native alone",
                "Asian alone", "Native Hawaiian and Other Pacific Islander alone", "Two or more races",
                "White alone or in combination", "Black alone or in combination", "American Indian and Alaska Native alone or in combination",
                "Asian alone or in combination", "Native Hawaiian and Other Pacific Islander alone or in combination"
      )
    )
  )
  names_v <- names(categories)

  if(is.null(name) & !get_names){
    stop("The 'name' parameter is required. Set 'get_names' parameter to TRUE to get the current names.")
  }else if(get_names){
    return(names_v)
  }

  idx <- which(names_v == name)

  if(length(idx) == 0){
    return(NULL)
  }else{
    if(is.null(start_idx)){
      start_idx = 1
    }
    if(is.null(end_idx)){
      end_idx = length(categories[[idx]]$val)
    }
    return(categories[[idx]][start_idx:end_idx])
  }
}
