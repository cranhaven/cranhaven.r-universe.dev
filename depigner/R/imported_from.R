#' Packages imported by a package
#'
#' @param x (chr) the package you would like to see its imports
#' @param include_self (lgl, FALSE) do you like to include `x` itself in
#'   the output?
#'
#' @return (chr) vector of packages imported by `x`
#' @export
#'
#' @examples
#' imported_from("depigner")
imported_from <- function(x, include_self = FALSE) {

  find.package("depigner")
  miss <- identical(find.package(x, quiet = TRUE), character())

  if (miss) {
    ui_stop("
      The packages {usethis::ui_field(x)} must be installed.
      Please install it and retry."
    )
  }

  raw <- utils::packageDescription(x)[["Imports"]]
  if (length(raw) == 0L) return(x[include_self])

  imports <- strsplit(raw, ",")[[1L]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(
    X = strsplit(parsed, "\\s+"),
    FUN = "[[", 1L,
    FUN.VALUE = character(1L)
  )
  if (include_self) {
    names <- c(names, x)
  }
  sort(unique(names))
}
