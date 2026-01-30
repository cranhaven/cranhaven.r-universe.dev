# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' Rounds all numbers in the paygap data-frame
#'
#' This function all numbers to zero decimals, except the paygap (which is rounded to 2 decimals):
#' @param x paygap object (output of div::div_paygap())
#' @returns the paygap data-frame (tibble only, not the whole paygap object)
#' @export
#' @examples
#' d <- div_fake_team()
#' pg <- div_paygap(d)
#' div_round_paygap(pg)


div_round_paygap <- function(x) {
  if (class(x) != "paygap") stop("div ERROR: div_round_paygap() called with a non-paygap object.")
  pg_round <- x$data
  i <- 1
  for (k in x$roundable_cols) {
    pg_round[, k] <- round(x$data[, k], x$round_digits[i])
    i <- i + 1
  }
  return(pg_round)
}
