# Internal utility functions
# These are not exported — used by parse, validate, format, detect.

#' Normalize a phone number string
#'
#' Strips all non-digit characters except a leading `+`.
#'
#' @param x Character scalar.
#' @return Character scalar with only digits (and optional leading `+`).
#' @noRd
normalize_number <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  has_plus <- str_starts(x, fixed("+"))
  digits <- str_remove_all(x, "[^0-9]")
  if (!nzchar(digits)) return(NA_character_)
  if (has_plus) paste0("+", digits) else digits
}

#' Extract country code from a digit string (no leading +)
#'
#' Tries 1-, 2-, then 3-digit prefixes against the cc_to_regions index.
#'
#' @param digits Character scalar of digits (leading + already stripped).
#' @return A list with `country_code` (character) and `national_number` (character),
#'   or NULL if no match.
#' @noRd
extract_country_code <- function(digits) {
  cc_index <- the$metadata$cc_to_regions
  for (len in 1:3) {
    if (nchar(digits) <= len) next
    prefix <- str_sub(digits, 1, len)
    if (!is.null(cc_index[[prefix]])) {
      return(list(
        country_code = prefix,
        national_number = str_sub(digits, len + 1)
      ))
    }
  }
  NULL
}

#' Resolve region for a country code + national number
#'
#' For country codes shared by multiple regions (e.g., +1 for NANPA),
#' checks each territory's `leading_digits` pattern against the national number.
#' Falls back to the main country for the code.
#'
#' @param country_code Character scalar (e.g., "1", "44").
#' @param national_number Character scalar.
#' @return Character scalar region code (e.g., "US", "NZ").
#' @noRd
resolve_region <- function(country_code, national_number) {
  regions <- the$metadata$cc_to_regions[[country_code]]
  if (is.null(regions)) return(NA_character_)
  if (length(regions) == 1) return(regions)

  # First pass: check leading_digits (set at territory level for some shared codes)
  for (region in regions) {
    territory <- the$metadata$territories[[region]]
    ld <- territory$leading_digits
    if (!is.null(ld) && str_detect(national_number, paste0("^(?:", ld, ")"))) {
      return(region)
    }
  }

  # Second pass: for NANPA and similar shared codes without leading_digits,
  # check if the number matches any type pattern (mobile/fixed_line contain
  # territory-specific area code lists)
  for (region in regions) {
    territory <- the$metadata$territories[[region]]
    if (is.null(territory$leading_digits)) {
      if (matches_type(national_number, territory$mobile) ||
          matches_type(national_number, territory$fixed_line)) {
        return(region)
      }
    }
  }

  # Fall back to main country (first in sorted list)
  regions[1]
}

#' Strip national prefix from a number
#'
#' Removes the national dialling prefix (e.g., "0" in NZ/UK/AU) from the
#' start of a number.
#'
#' @param number Character scalar.
#' @param territory Territory list from metadata.
#' @return Character scalar with prefix removed.
#' @noRd
strip_national_prefix <- function(number, territory) {
  np <- territory$national_prefix
  if (is.null(np) || !nzchar(np)) return(number)

  if (str_starts(number, fixed(np))) {
    stripped <- str_sub(number, nchar(np) + 1)
    if (nzchar(stripped)) return(stripped)
  }

  number
}

#' Check if a national number has a valid length for a territory
#'
#' @param national_number Character scalar.
#' @param territory Territory list from metadata.
#' @return Logical.
#' @noRd
valid_length <- function(national_number, territory) {
  lengths <- territory$general_desc$possible_lengths
  if (is.null(lengths)) return(TRUE) # no length info = accept

  nchar(national_number) %in% lengths
}

#' Match a national number against a type pattern
#'
#' @param national_number Character scalar.
#' @param type_info List with `pattern` and `possible_lengths`.
#' @return Logical.
#' @noRd
matches_type <- function(national_number, type_info) {
  if (is.null(type_info) || is.null(type_info$pattern)) return(FALSE)
  # Check length first (fast rejection)
  if (!is.null(type_info$possible_lengths)) {
    if (!nchar(national_number) %in% type_info$possible_lengths) return(FALSE)
  }
  str_detect(national_number, type_info$pattern)
}

#' Find the matching format rule for a national number
#'
#' @param national_number Character scalar.
#' @param territory Territory list from metadata.
#' @return A format list, or NULL if no match.
#' @noRd
find_format <- function(national_number, territory) {
  formats <- territory$formats
  if (length(formats) == 0) return(NULL)

  for (fmt in formats) {
    # Check leading digits (last pattern is most specific)
    if (length(fmt$leading_digits_patterns) > 0) {
      last_ld <- fmt$leading_digits_patterns[length(fmt$leading_digits_patterns)]
      if (!str_detect(national_number, paste0("^(?:", last_ld, ")"))) {
        next
      }
    }
    # Check main pattern matches
    if (str_detect(national_number, paste0("^(?:", fmt$pattern, ")$"))) {
      return(fmt)
    }
  }

  NULL
}

#' Convert libphonenumber format string ($1, $2, ...) to stringr replacement (\\1, \\2, ...)
#' @noRd
to_r_replacement <- function(fmt_str) {
  str_replace_all(fmt_str, "\\$(\\d)", "\\\\\\1")
}

#' Apply a format rule to a national number
#'
#' @param national_number Character scalar.
#' @param fmt Format list from metadata.
#' @param include_national_prefix Logical. If TRUE, apply national prefix formatting rule.
#' @param territory Territory list (needed to get national_prefix value).
#' @return Character scalar formatted number.
#' @noRd
apply_format <- function(national_number, fmt, include_national_prefix = FALSE,
                         territory = NULL) {
  replacement <- to_r_replacement(fmt$format)
  formatted <- str_replace(
    national_number,
    paste0("^(?:", fmt$pattern, ")$"),
    replacement
  )

  if (include_national_prefix && !is.null(fmt$national_prefix_formatting_rule) &&
      !is.na(fmt$national_prefix_formatting_rule)) {
    npfr <- fmt$national_prefix_formatting_rule
    np <- if (!is.null(territory)) territory$national_prefix %||% "" else ""
    # $NP = national prefix, $FG = first group of formatted output
    first_group <- str_replace(formatted, "^([^ ]+).*", "\\1")
    rest <- str_replace(formatted, "^[^ ]+ ?", "")
    prefix_formatted <- str_replace_all(npfr, fixed("$NP"), np)
    prefix_formatted <- str_replace_all(prefix_formatted, fixed("$FG"), first_group)
    if (nzchar(rest) && rest != first_group) {
      formatted <- paste(prefix_formatted, rest)
    } else {
      formatted <- prefix_formatted
    }
  }

  formatted
}
