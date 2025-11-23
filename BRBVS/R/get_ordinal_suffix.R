#' Generate Ordinal Suffix for a Given Number
#'
#' @description
#' `get_ordinal_suffix` takes a number and returns its ordinal suffix.
#' Ordinal suffixes are "st" for first, "nd" for second, "rd" for third,
#' and "th" for all other numbers. Special cases from 11th to 13th are
#' also handled appropriately.
#'
#' @param number A positive integer for which an ordinal suffix is required.
#'
#' @return
#' A character string representing the ordinal suffix of the input number.
#' @noRd
get_ordinal_suffix <- function(number) {
  if (number %% 100 >= 11 && number %% 100 <= 13) {
    return("th")
  }
  suffixes <- c("st", "nd", "rd", rep("th", 7))
  return(suffixes[min(number %% 10, 9) + 1])
}
