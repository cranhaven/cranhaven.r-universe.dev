#' Validate Phone Numbers
#'
#' Checks whether phone numbers are valid according to libphonenumber's
#' metadata. A number is valid if it parses successfully, has a valid length,
#' and matches the general number pattern for its region.
#'
#' @param number Character vector of phone numbers.
#' @param default_region ISO 3166-1 alpha-2 region code for numbers in
#'   national format. See [phone_parse()] for details.
#'
#' @return Logical vector. `TRUE` for valid numbers, `FALSE` otherwise.
#'   `NA` inputs return `FALSE`.
#'
#' @examples
#' phone_valid("+64211234567")
#' phone_valid(c("+64211234567", "+6421", "not a number"))
#' phone_valid("021 123 4567", default_region = "NZ")
#'
#' @export
phone_valid <- function(number, default_region = NULL) {
  parsed <- phone_parse(number, default_region = default_region)
  vapply(parsed, function(p) isTRUE(p$valid), logical(1))
}
