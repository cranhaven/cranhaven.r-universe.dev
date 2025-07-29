#' Forms kValue matrix from list of kinship matrices
#'
#' A `kValue` matrix has one row for each pair of individuals in the kinship
#' matrix and one column for each kinship matrix.
#' A `kValue` matrix has one row for each pair of individuals in the kinship
#' matrix and one column for each kinship matrix. Thus, in a kinship matrix with
#' 20 individuals the kinship matrix will have 20 rows by 20 columns but only
#' the upper or lower triangle has unique information as the diagonal values are
#' by definition all 1.0 and the upper triangle has the same values as the
#' lower triangle. The `kValue` table will have 210 rows. The calculation for
#' the number or row in the `kValue` table is  \eqn{20 + (20 * 19) / 2}
#' rows with the 20 values from the kinship coeficient matrix diagonal and
#' \eqn{(20 * 19) / 2} elements from one of either of the two triangles.
#'
#' The `kValue` matrix for 1
#' kinship matrix for 20 individuals will have 210 rows and 3 columns. The
#' first two columns are dedicated to the ID pairs and the third column contains
#' the pair's kinship coefficient.
#'
#' Thus, the number of rows in the kValues matrix will
#'  be \eqn{n + n(n-1) / 2} and the number of columns will be 2 plus one
#'  additional column for each kinship matrix (\eqn{2 + n}).
#
#' @return Dataframe object with columns \code{id_1}, \code{id_2}, and one
#' \code{kinship} column for each kinship matrix in \code{kinshipMatricies}
#' where the first two columns contain the IDs of the
#' individuals in the kinship matrix provided to the function and the
#' \code{kinship} columms contain the corresponding kinship coefficients.
#' In contrast to the kinship matrix. Each possible pairing of IDs appears
#' once.
#'
#' @param kinshipMatrices list of square matrices of kinship values. May or
#' may not have named rows and columns.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::smallPed
#' simParent_1 <- list(
#'   id = "A",
#'   sires = c("s1_1", "s1_2", "s1_3"),
#'   dams = c("d1_1", "d1_2", "d1_3", "d1_4")
#' )
#' simParent_2 <- list(
#'   id = "B",
#'   sires = c("s1_1", "s1_2", "s1_3"),
#'   dams = c("d1_1", "d1_2", "d1_3", "d1_4")
#' )
#' simParent_3 <- list(
#'   id = "E",
#'   sires = c("A", "C", "s1_1"),
#'   dams = c("d3_1", "B")
#' )
#' simParent_4 <- list(
#'   id = "J",
#'   sires = c("A", "C", "s1_1"),
#'   dams = c("d3_1", "B")
#' )
#' simParent_5 <- list(
#'   id = "K",
#'   sires = c("A", "C", "s1_1"),
#'   dams = c("d3_1", "B")
#' )
#' simParent_6 <- list(
#'   id = "N",
#'   sires = c("A", "C", "s1_1"),
#'   dams = c("d3_1", "B")
#' )
#' allSimParents <- list(
#'   simParent_1, simParent_2, simParent_3,
#'   simParent_4, simParent_5, simParent_6
#' )
#'
#' extractKinship <- function(simKinships, id1, id2, simulation) {
#'   ids <- dimnames(simKinships[[simulation]])[[1]]
#'   simKinships[[simulation]][
#'     seq_along(ids)[ids == id1],
#'     seq_along(ids)[ids == id2]
#'   ]
#' }
#'
#' extractKValue <- function(kValue, id1, id2, simulation) {
#'   kValue[kValue$id_1 == id1 & kValue$id_2 == id2, paste0(
#'     "sim_",
#'     simulation
#'   )]
#' }
#'
#' n <- 10
#' simKinships <- createSimKinships(ped, allSimParents, pop = ped$id, n = n)
#' kValue <- kinshipMatricesToKValues(simKinships)
#' extractKValue(kValue, id1 = "A", id2 = "F", simulation = 1:n)
kinshipMatricesToKValues <- function(kinshipMatrices) {
  first <- TRUE
  for (i in seq_along(kinshipMatrices)) {
    if (first) {
      kValues <- kinshipMatrixToKValues(kinshipMatrices[[i]])
      first <- FALSE
    } else { # only need kinship value
      kValues <-
        cbind(
          kValues,
          kinshipMatrixToKValues(kinshipMatrices[[i]])[, "kinship"]
        )
    }
  }
  names(kValues) <- c(
    names(kValues[, 1L:2L]),
    paste0("sim_", seq_along(kinshipMatrices))
  )
  kValues
}
