#' Finding strings in a character vector using exact or partial search
#'
#' @param names Character vector
#' @param name Character vector to use as partial or exact search in names
#' @param exact logical, TRUE to find exact names, FALSE otherwise (default)
#'
#' @return Character vector of founds strings
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#'
#' names <- c("abscission", "alphaphot", "codelaitr", "dlaimax", "lai")
#'
#' find_names(names, "lai")
#'
#' #> [1] "codelaitr" "dlaimax"
#'
#' find_names(names, "lai", exact = TRUE)
#'
#' #> [1] "lai"
find_names <- function(names, name, exact = FALSE) {
  patt_str <- "x"
  if (exact) patt_str <- "^x$"

  names <- unique(
    unlist(lapply(
      name,
      function(x) {
        grep(
          x = names,
          pattern = gsub(pattern = "x", replacement = x, x = patt_str),
          value = TRUE
        )
      }
    ))
  )

  return(names)
}
