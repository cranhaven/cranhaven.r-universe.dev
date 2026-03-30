#' Get All Phone Number Information
#'
#' Parses, validates, formats, and classifies phone numbers in a single call.
#' Returns a data frame with one row per input number.
#'
#' @param number Character vector of phone numbers.
#' @param default_region ISO 3166-1 alpha-2 region code for numbers in
#'   national format. See [phone_parse()] for details.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{`raw`}{Original input.}
#'     \item{`e164`}{E.164 formatted number.}
#'     \item{`national`}{National format.}
#'     \item{`international`}{International format.}
#'     \item{`region`}{ISO 3166-1 alpha-2 region code.}
#'     \item{`country_code`}{Country calling code.}
#'     \item{`type`}{Number type (mobile, fixed_line, etc.).}
#'     \item{`valid`}{Logical validation result.}
#'   }
#'
#' @examples
#' phone_info("+64211234567")
#' phone_info(c("+64211234567", "+12125551234"))
#'
#' @export
phone_info <- function(number, default_region = NULL) {
  parsed <- phone_parse(number, default_region = default_region)

  data.frame(
    raw = vapply(parsed, function(p) p$raw %||% NA_character_, character(1)),
    e164 = vapply(parsed, function(p) format_single(p, "E164"), character(1)),
    national = vapply(parsed, function(p) format_single(p, "NATIONAL"), character(1)),
    international = vapply(parsed, function(p) format_single(p, "INTERNATIONAL"), character(1)),
    region = vapply(parsed, function(p) {
      if (isTRUE(p$valid)) p$region else NA_character_
    }, character(1)),
    country_code = vapply(parsed, function(p) {
      if (isTRUE(p$valid)) p$country_code else NA_character_
    }, character(1)),
    type = vapply(parsed, function(p) detect_type_single(p), character(1)),
    valid = vapply(parsed, function(p) isTRUE(p$valid), logical(1)),
    stringsAsFactors = FALSE
  )
}
