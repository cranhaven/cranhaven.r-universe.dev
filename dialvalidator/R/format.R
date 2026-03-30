#' Format Phone Numbers
#'
#' Formats phone numbers according to standard conventions.
#'
#' @param number Character vector of phone numbers.
#' @param format One of `"E164"` (default), `"NATIONAL"`, or `"INTERNATIONAL"`.
#'   \describe{
#'     \item{`E164`}{`+64211234567` — compact international format with no spaces.}
#'     \item{`NATIONAL`}{`021 123 4567` — local dialling format with national prefix.}
#'     \item{`INTERNATIONAL`}{`+64 21 123 4567` — international format with spaces.}
#'   }
#' @param default_region ISO 3166-1 alpha-2 region code for numbers in
#'   national format. See [phone_parse()] for details.
#'
#' @return Character vector of formatted numbers. Returns `NA` for numbers
#'   that cannot be parsed.
#'
#' @examples
#' phone_format("+64211234567")
#' phone_format("+64211234567", "NATIONAL")
#' phone_format("+64211234567", "INTERNATIONAL")
#'
#' @export
phone_format <- function(number, format = c("E164", "NATIONAL", "INTERNATIONAL"),
                         default_region = NULL) {
  format <- rlang::arg_match(format)
  parsed <- phone_parse(number, default_region = default_region)

  vapply(parsed, function(p) format_single(p, format), character(1))
}

#' Format a single parsed number
#' @noRd
format_single <- function(parsed, format) {
  if (!isTRUE(parsed$valid)) return(NA_character_)

  cc <- parsed$country_code
  nn <- parsed$national_number
  region <- parsed$region
  territory <- the$metadata$territories[[region]]

  switch(format,
    E164 = paste0("+", cc, nn),
    NATIONAL = format_national(nn, territory),
    INTERNATIONAL = format_international(nn, cc, territory)
  )
}

#' @noRd
format_national <- function(national_number, territory) {
  if (is.null(territory)) return(NA_character_)

  fmt <- find_format(national_number, territory)
  if (is.null(fmt)) {
    np <- territory$national_prefix %||% ""
    return(paste0(np, national_number))
  }

  apply_format(national_number, fmt, include_national_prefix = TRUE,
               territory = territory)
}

#' @noRd
format_international <- function(national_number, country_code, territory) {
  if (is.null(territory)) return(paste0("+", country_code, " ", national_number))

  fmt <- find_format(national_number, territory)
  if (is.null(fmt)) {
    return(paste0("+", country_code, " ", national_number))
  }

  # Use intl_format if available, otherwise standard format without national prefix
  fmt_str <- if (!is.null(fmt$intl_format) && fmt$intl_format != "NA") {
    fmt$intl_format
  } else {
    fmt$format
  }

  replacement <- to_r_replacement(fmt_str)
  formatted <- str_replace(
    national_number,
    paste0("^(?:", fmt$pattern, ")$"),
    replacement
  )

  paste0("+", country_code, " ", formatted)
}
