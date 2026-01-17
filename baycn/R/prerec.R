# undirected
#
# Takes a posterior probability adjacency matrix and, with a given cutoff,
# creates an undirected adjacency matrix.
#
# @param pam A posterior probability adjacency matrix.
#
# @param cutoff A number between 0 and 1 indicating the posterior probability
# threshold for considering an edge present.
#
# @return An undirected adjacency matrix.
#
undirected <- function (pam,
                        cutoff) {

  # Create an empty matrix that will become the undirected adjacency matrix.
  amUndir <- matrix(0,
                    nrow = nrow(pam),
                    ncol = ncol(pam))

  # Loop through the posterior probability matrix and add the posterior for the
  # two directions. If it is larger than the cutoff add an undirected edge in
  # the appropriate places in the adjacency matrix.
  for (e in 1:nrow(pam)) {

    for (v in 1:ncol(pam)) {

      # Test if the sum of the two directions is greater than the cutoff.
      if (e < v && (sum(pam[e, v], pam[v, e]) > cutoff)) {

        # Create an undirected edge in the undirected adjacency matrix.
        amUndir[e, v] <- 1
        amUndir[v, e] <- 1

      }

    }

  }

  return (amUndir)

}

#' prerec
#'
#' Calculates the precision and recall (i.e., power) of the inferred graph.
#'
#' @param amInferred A baycn object or a posterior probability adjacency
#' matrix.
#'
#' @param amTrue The undirected adjacency matrix of the true graph. This will
#' be a symmetric matrix with 0s along the diagonal.
#'
#' @param cutoff A number between 0 and 1 indicating the posterior probability
#' threshold for considering an edge present.
#'
#' @return A list. The first element is the precision and the second element is
#' the recall of the inferred graph.
#'
#' @examples
#'
#' set.seed(5)
#'
#' # Generate data from topology GN4.
#' data_gn4 <- simdata(graph = 'gn4',
#'                     N = 200,
#'                     b0 = 0,
#'                     ss = 1,
#'                     s = 1)
#'
#' # Adjacency matrix for topology GN4 - all possible edges.
#' am_gn4 <- matrix(c(0, 1, 1, 1,
#'                    0, 0, 1, 1,
#'                    0, 0, 0, 1,
#'                    0, 0, 0, 0),
#'                  byrow = TRUE,
#'                  nrow = 4)
#'
#' # Run baycn on the data from topology GN4.
#' baycn_gn4 <- mhEdge(data = data_gn4,
#'                     adjMatrix = am_gn4,
#'                     prior = c(0.05,
#'                               0.05,
#'                               0.9),
#'                     nCPh = 0,
#'                     nGV = 0,
#'                     pmr = FALSE,
#'                     iterations = 1000,
#'                     burnIn = 0.2,
#'                     thinTo = 500,
#'                     progress = FALSE)
#'
#' # Adjacency matrix with the true edges for topology GN4.
#' am_gn4_true <- matrix(c(0, 1, 1, 0,
#'                         1, 0, 0, 1,
#'                         1, 0, 0, 1,
#'                         0, 1, 1, 0),
#'                       byrow = TRUE,
#'                       nrow = 4)
#'
#' # Calculate the precision and recall.
#' prerec_gn4 <- prerec(amInferred = baycn_gn4,
#'                      amTrue = am_gn4_true,
#'                      cutoff = 0.4)
#'
#' @export
#'
prerec <- function (amInferred,
                    amTrue,
                    cutoff) {

  # Check if the input for am_inferred is a baycn object.
  if (inherits(amInferred, 'baycn')) {

    # Create an undirected adjacency matrix from the inferred adjacency matrix
    # when the input is a baycn object.
    amUI <- undirected(pam = amInferred@posteriorPM,
                       cutoff = cutoff)

  } else {

    # Create an undirected adjacency matrix from the inferred adjacency matrix.
    amUI <- undirected(pam = amInferred,
                       cutoff = cutoff)

  }

  # Extract the upper triangular matrix of the true graph's adjacency matrix.
  edgTrue <- amTrue[upper.tri(amTrue)]

  # Extract the upper triangular matrix of the inferred graph's adjacency
  # matrix.
  edgInferred <- amUI[upper.tri(amUI)]

  # Determine which edges were correctly identified.
  nCorrect <- sum(edgInferred == 1 & edgTrue == 1)

  # Calculate the total number of inferred edges.
  iTotal <- sum(edgInferred)

  # Calculate the total number of true edges.
  tTotal <- sum(edgTrue)

  # Calculate the precision of the graph.
  precision <- ifelse(test = iTotal == 0,
                      yes = 0,
                      no = nCorrect / iTotal)

  # Calculate the recall of the graph.
  recall <- nCorrect / tTotal

  return (list(precision = precision,
               recall = recall))

}
