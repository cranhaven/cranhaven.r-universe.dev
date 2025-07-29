#' Generates a kinship matrix.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' The function previously had an internal call to the kindepth function in
#' order to provide the parameter pdepth (the generation number). This version
#' requires the generation number to be calculated elsewhere and passed into
#' the function.
#'
#' The rows (cols) of founders are just 0.5 * identity matrix, no further
#'    processing is needed for them.
#' Parents must be processed before their children, and then a child's
#'    kinship is just a sum of the kinship's for his or her parents.
#'
#' @return A kinship square matrix
#'
#' @param id character vector of IDs for a set of animals.
#' @param father.id character vector or NA for the IDs of the sires for the set
#' of animals.
#' @param mother.id character vector or NA for the IDs of the dams for the set
#' of animals.
#' @param pdepth integer vector indicating the generation number for each
#' animal.
#' @param sparse logical flag. If \code{TRUE}, \code{Matrix::Diagnol()} is
#' used to make a unit diagonal matrix. If \code{FALSE}, \code{base::diag()} is
#' used to make a unit square matrix.
#'
#' @description \{Kinship Matrix Functions\} \{
#' The code for the kinship function was written by Terry Therneau
#' at the Mayo clinic and taken from his website. This function is part of a
#' package written in S (and later ported to R) for calculating kinship and
#' other statistics.
#' \}
#'
#' @author \{Terry M. Therneau, Mayo Clinic (mayo.edu), original version\}
#'
#'
#' @references \{S-Plus/R Function Page\}
# nolint start: line_length_linter
#' \emph{www.mayo.edu/research/departments-divisions/department-health-sciences-research/division-biomedical-statistics-informatics/software/}
# nolint end: line_length_linter
#'  @description \{s-plus-r-functions\} \{Downloaded 2014-08-26\}
#'  This page address is now (2019-10-03) stale.
#'
#' All of the code on the S-Plus page was stated to be released under the
#' GNU General Public License (version 2 or later).
#'
#' The R version became the kinship2 package available on CRAN:
#' @references \url{https://cran.r-project.org/package=kinship2}
#'
#' $Id: kinship.s,v 1.5 2003/01/04 19:07:53 therneau Exp $
#'
#' @references \{Create the kinship matrix, using the algorithm of K Lange,
#'  Mathematical and Statistical Methods for Genetic Analysis,
#'  Springer, 1997, p 71-72.\}
#'
#' @author \{as modified by, M Raboin, 2014-09-08 14:44:26\}
#'
#' @import Matrix
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::lacy1989Ped
#' ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
#' kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen)
#' ped
#' kmat
kinship <- function(id, father.id, mother.id, pdepth, sparse = FALSE) { # nolint: object_name_linter
  # Returns: Matrix (row and col names are 'id')
  n <- length(id)
  if (anyDuplicated(id)) {
    stop("All id values must be unique")
  }
  if (sparse) {
    kmat <- Diagonal(n + 1L) / 2L
  } else {
    kmat <- diag(n + 1L) / 2L
  }

  kmat[n + 1L, n + 1L] <- 0L # if A and B both have "unknown" dad, this ensures
  # that they won't end up 'related' in the matrix

  # id number "n + 1" is a placeholder for missing parents
  mrow <- match(mother.id, id, nomatch = n + 1L) # row number of the mother
  drow <- match(father.id, id, nomatch = n + 1L) # row number of the dad

  # Those at depth == 0 don't need to be processed
  # Subjects with depth = i must be processed before those at depth i + 1.
  # Any parent is guarranteed to be at a lower depth than their children
  #  The inner loop on "i" can NOT be replaced with a vectorized expression:
  # sibs' effect on each other is cumulative.
  for (depth in 1L:max(pdepth, na.rm = TRUE)) {
    indx <- (1L:n)[pdepth == depth]
    for (i in indx) {
      mom <- mrow[i]
      dad <- drow[i]
      kmat[i, ] <- kmat[, i] <- (kmat[mom, ] + kmat[dad, ]) / 2L
      kmat[i, i] <- (1L + kmat[mom, dad]) / 2L
    }
  }

  kmat <- kmat[1L:n, 1L:n]
  dimnames(kmat) <- list(id, id)
  kmat
}
