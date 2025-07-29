#' Calculates the mean kinship for each animal in a kinship matrix
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Genetic Value Analysis
#'
#' The mean kinship of animal \emph{i} is \deqn{MK_i = \Sigma f_ij / N},
#' in which the summation is over all animals, \emph{j}, including the kinship
#' of animal \emph{i} to itself.
#'
#' @return A named numeric vector of average kinship coefficients for each
#' animal ID. Elements are named with the IDs from the columns of kmat.
#'
#' @param kmat a numeric matrix of pairwise kinship coefficients.
#' Animal IDs are the row and column names.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::qcPed
#' kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen)
#' head(meanKinship(kmat))
meanKinship <- function(kmat) {
  colMeans(kmat, na.rm = TRUE)
}
