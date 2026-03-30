#' Detect Phone Number Type
#'
#' Classifies phone numbers as mobile, fixed-line, toll-free, etc. by
#' matching against libphonenumber's per-territory type patterns.
#'
#' @param number Character vector of phone numbers.
#' @param default_region ISO 3166-1 alpha-2 region code for numbers in
#'   national format. See [phone_parse()] for details.
#'
#' @return Character vector of phone number types. Possible values:
#'   `"mobile"`, `"fixed_line"`, `"fixed_line_or_mobile"`, `"toll_free"`,
#'   `"premium_rate"`, `"shared_cost"`, `"personal_number"`, `"voip"`,
#'   `"pager"`, `"uan"`, `"voicemail"`, `"unknown"`.
#'   Returns `NA` for numbers that cannot be parsed.
#'
#' @examples
#' phone_type("+64211234567")
#' phone_type(c("+6493001234", "+64800123456"))
#'
#' @export
phone_type <- function(number, default_region = NULL) {
  parsed <- phone_parse(number, default_region = default_region)
  vapply(parsed, function(p) detect_type_single(p), character(1))
}

#' Detect Country of a Phone Number
#'
#' Returns the ISO 3166-1 alpha-2 region code for each phone number.
#'
#' @param number Character vector of phone numbers.
#' @param default_region ISO 3166-1 alpha-2 region code for numbers in
#'   national format. See [phone_parse()] for details.
#'
#' @return Character vector of region codes (e.g., `"NZ"`, `"US"`).
#'   Returns `NA` for numbers that cannot be parsed.
#'
#' @examples
#' phone_country("+64211234567")
#' phone_country(c("+12125551234", "+442071234567"))
#'
#' @export
phone_country <- function(number, default_region = NULL) {
  parsed <- phone_parse(number, default_region = default_region)
  vapply(parsed, function(p) {
    if (isTRUE(p$valid)) p$region else NA_character_
  }, character(1))
}

#' Detect type for a single parsed number
#' @noRd
detect_type_single <- function(parsed) {
  if (!isTRUE(parsed$valid)) return(NA_character_)

  territory <- the$metadata$territories[[parsed$region]]
  if (is.null(territory)) return(NA_character_)

  nn <- parsed$national_number

  # Check types in priority order
  type_checks <- c(
    "toll_free", "premium_rate", "shared_cost", "personal_number",
    "voip", "pager", "uan", "voicemail"
  )

  # Check mobile and fixed_line first (most common)
  is_mobile <- matches_type(nn, territory$mobile)
  is_fixed <- matches_type(nn, territory$fixed_line)

  if (is_mobile && is_fixed) return("fixed_line_or_mobile")
  if (is_mobile) return("mobile")
  if (is_fixed) return("fixed_line")

  # Check remaining types
  for (type in type_checks) {
    if (matches_type(nn, territory[[type]])) return(type)
  }

  "unknown"
}
