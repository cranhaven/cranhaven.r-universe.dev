#' Combines two vectors of alleles when alleles are character vectors.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Combines two vectors by randomly selecting one allele
#' or the other at each position. Alleles may be of any class that
#' does not require attributes as the vectors are combined with \code{c()}.
#'
#' The current implementation is slower than the one using integer vectors
#' (\code{chooseAlleles}).
#'
#' @return An integer vector with the result of sampling from \code{a1}
#' and \code{a2} according to Mendelian inheritance.
#'
#' @param a1 vector with first parent alleles for each individual
#' @param a2 vector with second parent alleles for each individual
#' \code{a1} and \code{a2} are equal length vectors of alleles for one
#' individual
#' @noRd
chooseAllelesChar <- function(a1, a2) {
  s <- sample.int((2L * length(a1)), length(a1), replace = FALSE)
  c(a1, a2)[s]
}
