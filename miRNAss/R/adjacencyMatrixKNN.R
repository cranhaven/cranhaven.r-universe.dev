#' MiRNAss: Genome-wide pre-miRNA discovery from few labeled examples
#'
#' This funtions builds the adjacency matrix (the graph) given a data frame
#' of numerical features.
#' @param sequenceFeatures Data frame with features extracted from stem-loop
#' sequences.
#' @param sequenceLabels Vector of labels of the stem-loop sequences. It must
#'  have -1 for negative examples, 1 for known miRNAs and zero for the unknown
#'  sequences (the ones that would be classificated).
#' @param nNearestNeighbor Number of nearest neighbors in the KNN graph. The
#' default value is 10.
#' @param threadNumber Number of threads used for the calculations. If it is NA
#' leave OpenMP decide the number (may vary across different platforms).
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
#'
#' for (nev in seq(50,200, 50)) {
#'     # the data frame of features 'x' should not be pass as parameter
#'     p = miRNAss(sequenceLabels = y, AdjMatrix = A,
#'                 nEigenVectors = nev)
#'     # Calculate some performance measures
#'     SE = mean(p[ celegans$CLASS & y==0] > 0)
#'     SP = mean(p[!celegans$CLASS & y==0] < 0)
#'     cat("N: ", nev, "\n  SE: ", SE, "\n  SP:   ", SP, "\n")
#' }
#' @import Matrix
#' @importFrom Rcpp evalCpp
#' @useDynLib miRNAss
#' @export
adjacencyMatrixKNN <- function(sequenceFeatures,
                               sequenceLabels = rep(0, nrow(sequenceFeatures)),
                               nNearestNeighbor = 10,
                               threadNumber = NA) {
    if (!is.matrix(sequenceFeatures))
        sequenceFeatures <- as.matrix(sequenceFeatures)
    if (is.na(threadNumber))
        threadNumber <- 0
    el <- .edgeListKnn(sequenceFeatures, sequenceLabels, nNearestNeighbor, threadNumber)
    res <- sparseMatrix(
        i = c(el[, 1], el[, 2]),
        j = c(el[, 2], el[, 1]),
        x = c(el[, 3], el[, 3]),
        use.last.ij = TRUE,
        symmetric = FALSE,
        dims = rep(nrow(sequenceFeatures), 2)
    )
    return(res)
}
