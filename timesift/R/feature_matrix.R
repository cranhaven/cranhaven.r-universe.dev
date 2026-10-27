#' Bring an already-reduced feature table into a ladder
#'
#' A representation the package did not build, such as a published set of hand-aggregated climate
#' summaries, enters here. It becomes a one-channel `[unit, feature, 1]` array, which is what a
#' learner reads, so a feature table and a temporal grain can be arms of the same [grain_ladder()]
#' and be scored on the same cells by the same rule.
#'
#' It carries no time axis, because it has none: the reduction already happened, elsewhere, and
#' what reaches the model is a list of numbers per unit. That is the whole point of comparing
#' against it.
#'
#' @param m A matrix or data frame of units by features, with unit identifiers in the row names or
#'   in a leading character or factor column.
#' @param label The name the arm is reported under.
#'
#' @return A `timesift_matrix` of shape `[unit, feature, 1]`.
#'
#' @examples
#' m <- matrix(rnorm(30), nrow = 10,
#'             dimnames = list(sprintf("p%02d", 1:10), paste0("bio", 1:3)))
#' feature_matrix(m)
#'
#' @export
feature_matrix <- function(m, label = "features") {
  m <- .as_response(m)
  out <- array(as.numeric(m), dim = c(nrow(m), ncol(m), 1L),
               dimnames = list(rownames(m), colnames(m), label))
  attr(out, "grain") <- label
  attr(out, "stats") <- label
  attr(out, "year_start") <- NA_character_
  attr(out, "bin_start") <- .POSIXct(rep(NA_real_, ncol(m)), tz = "UTC")
  attr(out, "bin_end") <- .POSIXct(rep(NA_real_, ncol(m)), tz = "UTC")
  attr(out, "bin_n") <- matrix(NA_integer_, nrow = nrow(m), ncol = ncol(m),
                               dimnames = dimnames(m))
  attr(out, "bin_partial") <- rep(FALSE, ncol(m))
  class(out) <- c("timesift_matrix", "array")
  out
}
