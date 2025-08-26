#' Compare two biclusterings or a pair of partition matrices
#'
#' @param bc1 the first biclustering or partition matrix. Must be either of class
#'   \code{biclustermd} or \code{matrix}.
#' @param bc2 the second biclustering or partition matrix. Must be either of class
#'   \code{biclustermd} or \code{matrix}.
#'
#' @export
#'
#' @importFrom phyclust RRand
#'
#' @return If comparing a pair of biclusterings, a list containing the column
#'   similarity indices and the row similarity indices, in that order. If a pair of matrices,
#'   a vector of similarity indices.
#'
#' @examples
#' data("synthetic")
#' bc <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2)
#' bc2 <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2)
#'
#' # compare the two biclusterings
#' compare_biclusters(bc, bc2)
#'
#' # determine the similarity between initial and final row clusterings
#' compare_biclusters(bc$Q0, bc$Q)
#'
compare_biclusters <- function(bc1, bc2) {

  if(inherits(bc1, "biclustermd") & inherits(bc2, "biclustermd")) {

    P1 <- part_matrix_to_vector(bc1$P)
    P2 <- part_matrix_to_vector(bc2$P)

    Q1 <- part_matrix_to_vector(bc1$Q)
    Q2 <- part_matrix_to_vector(bc2$Q)

    P_similarity <- c(
      "Rand" = RRand(P1, P2)[[1]],
      "HA" = RRand(P1, P2)[[2]],
      "Jaccard" = jaccard_similarity(P1, P2)
    )
    Q_similarity <- c(
      "Rand" = RRand(Q1, Q2)[[1]],
      "HA" = RRand(Q1, Q2)[[2]],
      "Jaccard" = jaccard_similarity(Q1, Q2)
    )

    # list(
    #   "P Similarity" = adjustedRand(P1, P2, randMethod = c("Rand", "HA", "Jaccard")),
    #   "Q Similarity" = adjustedRand(Q1, Q2, randMethod = c("Rand", "HA", "Jaccard"))
    # )

    list(
      "P Similarity" = P_similarity,
      "Q Similarity" = Q_similarity
    )

  } else if(inherits(bc1, "matrix") & inherits(bc2, "matrix")) {

    # adjustedRand(
    #   part_matrix_to_vector(bc1),
    #   part_matrix_to_vector(bc2),
    #   randMethod = c("Rand", "HA", "Jaccard")
    # )

    c(
      "Rand" = RRand(part_matrix_to_vector(bc1), part_matrix_to_vector(bc2))[[1]],
      "HA" = RRand(part_matrix_to_vector(bc1), part_matrix_to_vector(bc2))[[2]],
      "Jaccard" = jaccard_similarity(part_matrix_to_vector(bc1), part_matrix_to_vector(bc2))
    )

  }

}
