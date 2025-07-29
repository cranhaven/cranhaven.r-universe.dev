#' Combines two vectors of alleles by randomly selecting one allele
#' or the other at each position.
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return An integer vector with the result of sampling from \code{a1}
#' and \code{a2} according to Mendelian inheritance.
#'
#' @param a1 integer vector with first allele for each individual
#' @param a2 integer vector with second allele for each individual
#' \code{a1} and \code{a2} are equal length vectors of alleles for one
#' individual
#' @export
#' @examples
#' chooseAlleles(0L:4L, 5L:9L)
chooseAlleles <- function(a1, a2) {
  s1 <- sample(c(0L, 1L), length(a1), replace = TRUE)
  s2 <- 1L - s1

  (a1 * s1) + (a2 * s2)
}
