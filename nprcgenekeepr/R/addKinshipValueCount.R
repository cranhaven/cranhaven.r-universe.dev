#' Add to count the number of occurrences of a kinship value seen for a pair of
#' individuals in one or more simulated pedigrees.
#'
#' Order of the mating pairs of individual is assumed to be identical among the
#' three lists \code{cKVC}, \code{kValues}, and
#' \code{kCounts}.
#'
#' @return Integer value of the number of occurrences of a specific kinship
#'         value seen for a pair of individuals in one or more simulated
#'         pedigrees.
#'
#' @param cKVC list object of cummulated Kinship Value Counts with same
#' structure as that returned by this function.
#' @param kValues list of unique kValues found in one set of
#' kinship matricies.
#' @param kCounts list of unique kCounts found in one set of
#' kinship matricies.
#' @param index list index for the set of kValues to compare and count
#' that is common among the three lists \code{cKVC},
#' \code{kValues}, and \code{kCounts}.
#' @param value one element of vector of unique kinship values at position
#' \code{index} within \code{cKVC}.
#' @noRd
addKinshipValueCount <- function(cKVC,
                                 kValues,
                                 kCounts,
                                 index, value) {
  if (any(kValues[[index]] == value)) {
    cKVC$kCounts[[index]][cKVC$kValues[[index]] == value] <-
      cKVC$kCounts[[index]][cKVC$kValues[[index]] == value] +
      kCounts[[index]][kValues[[index]] == value]
  }
  cKVC
}
