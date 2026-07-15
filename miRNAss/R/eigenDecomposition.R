#' MiRNAss: Genome-wide pre-miRNA discovery from few labeled examples
#'
#' This funtions calculate the eigenvectors and eigen values of the Laplacian
#' of the graph. As this proccess is quite time comsumin, this functions allows
#' to obtain this decomposition once and the be able to run miRNAss several times
#' in shorter times.
#' @param AdjMatrix Adjacency sparse matrix of the graph.
#' @param nEigenVectors Number of eigen vectors.
#' @return Returns the eigen descomposition as a list with two elements:
#' The eigen vectors matrix 'U' and the eigen values vector 'D'.
#' @examples
#' # First construct the label vector with the CLASS column
#' y = as.numeric(celegans$CLASS)*2 - 1
#'
#' # Remove some labels to make a test
#' y[sample(which(y>0),200)] = 0
#' y[sample(which(y<0),700)] = 0
#'
#' # Take all the features but remove the label column
#' x = subset(celegans, select = -CLASS)
#'
#' A = adjacencyMatrixKNN(x, y, 10, 8)
#' E = eigenDecomposition(AdjMatrix = A, nEigenVectors = 100)
#' for (mp in c(0.1,1,10)) {
#'     p = miRNAss(sequenceLabels = y, AdjMatrix = A,
#'                 eigenVectors = E, missPenalization = mp)
#'     # Calculate some performance measures
#'     SE = mean(p[ celegans$CLASS & y==0] > 0)
#'     SP = mean(p[!celegans$CLASS & y==0] < 0)
#'     cat("mP: ", mp, "\n  SE: ", SE, "\n  SP:   ", SP, "\n")
#' }
#' @import Matrix
#' @importFrom RSpectra eigs_sym eigs
#' @export
eigenDecomposition <- function(AdjMatrix, nEigenVectors) {
    nx <- nrow(AdjMatrix)
    D  <-   sparseMatrix(i = seq(1, nx),
                         j = seq(1, nx),
                         x = 1 / sqrt(colSums(AdjMatrix)))
    Lnorm <- sparseMatrix(   i = seq(1, nx),
                             j = seq(1, nx),
                             x = rep(1, nx)) - D %*% AdjMatrix %*% D

    lambda <- eigs_sym(
        A = Lnorm,
        k = nEigenVectors + 1,
        which = "SM",
        opts = list(tol = 1e-6)
    )

    U <- lambda$vectors[, seq(nEigenVectors, 1, -1)]
    D <- lambda$values[seq(nEigenVectors, 1, -1)]
    return(list(U = U, D = D))
}
