#' Converts the ancestry information to a standardized code
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Pedigree Curation
#'
#'
#' @return A factor vector of standardized designators specifying if an animal
#' is a Chinese rhesus, Indian rhesus, Chinese-Indian hybrid rhesus, or
#' Japanese macaque. Levels: CHINESE, INDIAN, HYBRID, JAPANESE, OTHER, UNKNOWN.
#'
#' @param ancestry character vector or NA with free-form text providing
#' information about the geographic population of origin.
#' @export
#' @examples
#' original <- c("china", "india", "hybridized", NA, "human", "gorilla")
#' convertAncestry(original)
convertAncestry <- function(ancestry) {
  ancestry <- tolower(ancestry)

  # Find entries containing non-standardized indications of population
  chinese <- grepl("chin", ancestry, fixed = TRUE) &
    !grepl("ind", ancestry, fixed = TRUE)
  indian <- !grepl("chin", ancestry, fixed = TRUE) &
    grepl("ind", ancestry, fixed = TRUE)
  hybrid <- ((grepl("chin", ancestry, fixed = TRUE) &
                grepl("ind", ancestry, fixed = TRUE)) |
    grepl("hyb", ancestry, fixed = TRUE))
  japanese <- grepl("jap", ancestry, fixed = TRUE)
  unknown <- is.na(ancestry)

  other <- !(chinese | indian | hybrid | japanese) & !unknown

  ancestry[chinese] <- "CHINESE"
  ancestry[indian] <- "INDIAN"
  ancestry[hybrid] <- "HYBRID"
  ancestry[japanese] <- "JAPANESE"
  ancestry[unknown] <- "UNKNOWN"
  ancestry[other] <- "OTHER"

  ancestry <- factor(ancestry, levels = c(
    "CHINESE", "INDIAN", "HYBRID",
    "JAPANESE", "OTHER", "UNKNOWN"
  ))
  ancestry
}
