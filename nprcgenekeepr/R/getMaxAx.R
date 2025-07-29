#' Set the scale the pyramid plot symmetrically
#'
#' Get the maximum of the absolute values of the negative (males) and positive
#' (female) animal counts and then round that up to the nearest multiple of the
#' modulus greater than or equal to the maximum value.
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Integer value equal to the nearest multiple of \code{axModulus} that
#' is greater than or equal to the maximum of the absolute values of the
#' negative (males) and positive (female) animal counts.
#' @param bins integer vector with numbers of individuals in each bin
#' @param axModulus integer value used in the modulus function to determine
#' the interval between possible maxAx values.
#'
#' @noRd
getMaxAx <- function(bins, axModulus) {
  makeRoundUp(max(max(bins$male), max(bins$female)), axModulus)
}
