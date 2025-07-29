#' Converts the fromCenter information to a standardized code
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Pedigree Curation
#'
#'
#' @return A logical vector specifying TRUE if an animal
#' is from the center otherwise FALSE.
#'
#' @param fromCenter character or logical vector or NA indicating whether or
#' not the animal is from the center.
#' @importFrom stringi stri_c stri_detect_fixed
#' @export
#' @examples
#' original <- c(
#'   "y", "yes", "Y", "Yes", "YES", "n", "N", "No", "NO", "no",
#'   "t", "T", "True", "true", "TRUE", "f", "F", "false", "False",
#'   "FALSE"
#' )
#' convertFromCenter(original)
convertFromCenter <- function(fromCenter) {
  trueValues <- c("Y", "YES", "T", "TRUE")
  falseValues <- c("N", "NO", "F", "FALSE")

  # grepl takes comparison to NA as a failure resulting in FALSE
  # We conpensate for that while looking for ambiguous text and then put the
  # NA values back before returning.
  if (!is.logical(fromCenter)) {
    fromCenterTrue <- grepl(paste(trueValues, collapse = "|"), fromCenter,
      ignore.case = TRUE
    )
    fromCenterFalse <- grepl(paste(falseValues, collapse = "|"), fromCenter,
      ignore.case = TRUE
    )
    fromCenterFalse[is.na(fromCenter)] <- TRUE
    if (any(fromCenterTrue == fromCenterFalse)) {
      stop(
        "fromCenter field has ambiguous values in row(s) ",
        get_and_or_list(seq_along(fromCenter)[
          (fromCenterTrue == fromCenterFalse)
        ])
      )
    } else {
      fromCenterTrue[is.na(fromCenter)] <- NA
      fromCenter <- fromCenterTrue
    }
  }
  fromCenter
}
