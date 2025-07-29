#' Converts sex indicator for an individual to a standardized codes.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Pedigree Curation
#'
#'
#' Standard sex codes are
#' \itemize{
#' \item\{F\} \{-- replacing "FEMALE" or "2"\}
#' \item\{M\} \{-- replacing "MALE" or "1"\}
#' \item\{H\} \{-- replacing "HERMAPHRODITE" or "4", if ignore.herm == FALSE\}
#' \item\{U\} \{-- replacing "HERMAPHRODITE" or "4", if ignore.herm == TRUE\}
#' \item\{U\} \{-- replacing "UNKNOWN" or "3"\}}
#'
#' @return A vector of factors representing standardized sex codes after
#' transformation from non-standard codes.
#'
#' @param sex factor with levels: "M", "F", "U". Sex specifier for an
#' individual.
#' @param ignoreHerm logical flag indicating if hermaphrodites should be
#' treated as unknown sex ("U"), default is \code{TRUE}.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' original <- c(
#'   "m", "male", "1", "MALE", "M", "F", "f", "female",
#'   "FemAle", "U", "Unknown", "H", "hermaphrodite",
#'   "U", "Unknown", "3", "4"
#' )
#' sexCodes <- convertSexCodes(original)
#' sexCodes
convertSexCodes <- function(sex, ignoreHerm = TRUE) {
  sex <- toupper(sex)
  sex[is.na(sex)] <- "U"

  sex[sex %in% c("MALE", "M", "1")] <- "M"
  sex[sex %in% c("FEMALE", "F", "2")] <- "F"
  sex[sex %in% c("UNKNOWN", "U", "3")] <- "U"

  if (ignoreHerm) {
    sex[sex %in% c("HERMAPHRODITE", "H", "4")] <- "U"
  } else {
    sex[sex %in% c("HERMAPHRODITE", "H", "4")] <- "H"
  }
  sex <- factor(sex, levels = c("F", "M", "H", "U"))
  return(sex)
}
