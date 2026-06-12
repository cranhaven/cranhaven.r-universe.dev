

panstarrs_user_agent <- function() {
  httr::user_agent(paste0(
    "panstarrs/", utils::packageVersion("panstarrs"), " ",
    "(https://CRAN.R-project.org/package=panstarrs)", " ",
    "httr/", utils::packageVersion("httr")
  ))
}

#' Check ra, dec params
#'
#' @param ra (degrees) Right Ascension
#' @param dec (degrees) Declination
#' @param .length length of coordinates. If `NULL` (default) then coordinates
#' can be any size.
#'
#' @return If the check is successful, the function returns nothing.
#'
#' @keywords internal
#'
validate_radec <- function(ra, dec, .length = NULL) {

  if (length(ra) != length(dec))
    stop(paste0(
      "Length of ra [", length(ra), "] ",
      "is not equal to length of dec [", length(dec), "]"
      )
    )


  if (! is.null(.length) && length(ra) != .length)
    stop(paste0("The length of the coordinates array must be", .length))

  checkmate::assert_numeric(ra, lower = 0, upper = 360, min.len = 0L)
  checkmate::assert_numeric(dec, lower = -90, upper = 90, min.len = 0L)

}






