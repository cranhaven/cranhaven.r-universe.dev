#' MiRNAss: Genome-wide pre-miRNA discovery from few labeled examples
#'
#' This is the main function of the miRNAss package and implements the miRNA
#' prediction method, It takes as main parameters a matrix with numerical
#' features extracted from RNA hairpins and an incomplent vector of labels
#' where the positive number represents known miRNAs, the negative are
#' not-miRNA hairpins and te zero values are unknown sequences (those that
#' will be classified). As a results it returns a complete label vector.
#' @param sequenceFeatures Data frame with features extracted from stem-loop
#' sequences. It is not required if the adjacency matrix is provided.
#' @param sequenceLabels Vector of labels of the stem-loop sequences. It must
#'  have -1 for negative examples, 1 for known miRNAs and zero for the unknown
#'  sequences (the ones that would be classificated).
#' @param AdjMatrix Sparse adjacency matrix representeing the graph.
#' If sequence features are provided it is ignored.
#' @param eigenVectors Eigen decomposition of the Laplacian matrix, as returned
#' by the function eigenDecomposition. If is not provided is calculated
#' internally (this parameter allows to calculate the eigen vectors once and then
#' run several times miRNAss with the same eigen vectors).
#' @param nEigenVectors Number of eigen vectors used to aproximate the solution
#' of the optimization problem. If the number is too low, smoother topographic
#' solutions are founded, probabily losing SP but achieving a better SE.
#' Generally, 400 are enought.
#' @param nNearestNeighbor Number of nearest neighbors in the KNN graph. The
#' default value is 10.
#' @param missPenalization Penalization of the missclassification of known
#' examples. The default value is 1. If the examples are not very confident,
#' this value can be diminished.
#' @param scallingMethod Method used for normalization and scalling of the
#' features. The options are 'none', 'whitening' and 'relief' (the default
#' option). The first option does nothing, the second calls the built-in
#' function 'scale' and the last one uses the ReliefFexpRank algorithm from
#' the coreLearn package.
#' @param positiveProp Expected proportion of positive sequences. If it is not
#' provided by the user, is estimated as sum(y > 0) / sum(y != 0) when there
#' are negative examples or as 2 * sum(y > 0) / sum(y == 0) when not.
#' @param neg2label Proportion of unlabeled stem-loops that would be labeled as
#' negative with the automatic method to start the classification algorithm.
#' The default is 0.05.
#' @param thresholdObjective Performance measure that would be optimized when
#' estimating the threshold. The options are 'Gm' (geometric mean of the SE and
#' the SP), 'G' (geometric mean of the SE and the precision), 'F1' (harmonic
#' mean between SE and the precision) and 'none' (do not calculate any
#' threshold). The default value is 'Gm'.
#' @param threadNumber Number of threads used for the calculations. If it is NA
#' leave OpenMP decide the number (may vary across different platforms).
#' @return Returns a vector with the same size of the input vector y with the
#' prediction scores for all sequences (even the labelled examples). If a
#' threshold Objective different from 'none' was set, the threshold is
#' estimated and subtracted from the scores, therefore the new threshold that
#' divide the classes is zero. Also, the positive scores are divided by the max
#' positive score, and the negative scores are divided by the magnitud of the
#' minimum negative score.
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
#' # Call miRNAss with default parameters
#' p = miRNAss(x,y)
#'
#' # Calculate some performance measures
#' SE = mean(p[ celegans$CLASS & y==0] > 0)
#' SP = mean(p[!celegans$CLASS & y==0] < 0)
#' cat("Sensitivity: ", SE, "\nSpecificity: ", SP, "\n")
#'
#' @import Matrix
#' @importFrom stats var
#' @importFrom CORElearn attrEval
#' @importFrom Rcpp evalCpp
#' @useDynLib miRNAss
#' @export
miRNAss =  function(sequenceFeatures = NULL, sequenceLabels, AdjMatrix = NULL,
                    nNearestNeighbor = 10, missPenalization = 1,
                    scallingMethod = "relief", thresholdObjective = "Gm",
                    neg2label = 0.05, positiveProp = NULL, eigenVectors = NULL,
                    nEigenVectors = min(400, round(length(sequenceLabels) / 5)),
                    threadNumber = NA) {
    nx <- length(sequenceLabels)
    nEigenVectors <- min(nEigenVectors, round(nx / 2))
    nNearestNeighbor <- min(nNearestNeighbor, nx / 4)

    if (any(sequenceLabels < 0)) {
        if (is.null(positiveProp))
            positiveProp <- sum(sequenceLabels > 0) / sum(sequenceLabels != 0)
    } else {
        if (is.null(positiveProp))
            positiveProp <- 2 * sum(sequenceLabels > 0) / length(sequenceLabels)

    }

    if (!is.null(sequenceFeatures)) {
        if (scallingMethod == "relief") {
            if (!any(sequenceLabels < 0)) {
                warning( paste0("Relief cannot be used without negative ",
                                "examples, switching to whitening"))
                sequenceFeatures <- scale(sequenceFeatures)
            } else {
                sequenceFeatures <- .reliefScalling(x = sequenceFeatures,
                                                    y = sequenceLabels,
                                                    nn = nNearestNeighbor)
            }
        } else if (scallingMethod == "whitening") {
            sequenceFeatures <- scale(sequenceFeatures)
        }
        else if (scallingMethod != "none") {
            stop("Invalid scalling method")
        }

        AdjMatrix <- adjacencyMatrixKNN(sequenceFeatures = sequenceFeatures,
                                        sequenceLabels = sequenceLabels,
                                        nNearestNeighbor = nNearestNeighbor,
                                        threadNumber = threadNumber)
    } else if (is.null(AdjMatrix))
        stop("Either sequenceFeatures or AdjMatrix must be provided.")

    if (is.null(eigenVectors))
        eigenVectors <- eigenDecomposition(AdjMatrix, nEigenVectors)


    if (!any(sequenceLabels < 0))
        sequenceLabels <- .searchNegatives(A = AdjMatrix,
                                           y = sequenceLabels,
                                           posFrac = positiveProp,
                                           prop2label = neg2label)

    pred <- .solveOptim(eigenVectors, sequenceLabels,
                        positiveProp, missPenalization)

    thresNumber = which(thresholdObjective == c("Gm", "G", "F1"))
    if (length(thresNumber) > 0)
        pred <- pred - .calcThreshold(pred, sequenceLabels, thresNumber)

    pos <- pred > 0
    pred[pos] <- pred[pos] / abs(max(pred[pos]))
    pred[!pos] <- pred[!pos] / abs(min(pred[!pos]))
    return(pred)
}

.searchNegatives <- function(A, y, prop2label, posFrac) {
    n2label <- ceiling(prop2label * (1 - posFrac) * sum(y == 0))
    d <- .calcDistance(
        p = attr(A, "p"),
        idx = attr(A, "i"),
        x = attr(A, "x"),
        y = y
    )
    p <- exp(1 - d) - 1
    sel <- sample(
        x = seq(1, length(y)),
        prob = p,
        replace = FALSE,
        size = n2label
    )
    y[sel] <- -1
    return(y)
}

.solveOptim <- function(desc, y, posFrac, missWeight) {
    nx <- length(y)
    posl <- which(y > 0)
    negl <- which(y < 0)

    target <- rep(0, nx)
    target[posl] <- sqrt((1 - posFrac) / posFrac)
    target[negl] <- -sqrt(posFrac / (1 - posFrac))

    C <- abs(y)

    G <- sweep(desc$U, MARGIN = 1, C, `*`)
    G <- missWeight * t(desc$U) %*% G
    diag(G) <- diag(G) + desc$D
    b <- as.matrix(C * target)
    b <- t(desc$U) %*% b
    b <- missWeight * b

    M <- rbind(cbind(G, -diag(
        nrow = nrow(G), ncol = ncol(G)
    )),
    cbind(-b %*% t(b) / nx, G))

    lambda <- Re(eigs(
        A = M,
        k = 1,
        which = "LM",
        sigma = 0,
        opts =
            list(maxitr = 10000, retvec = FALSE)
    )$values)

    w <- solve(G - diag(lambda, nrow(G), ncol(G)), b)
    return(desc$U %*% w)
}

.reliefScalling <- function(x, y, nn) {
    x <- x[, apply(  X = x,
                     MARGIN = 2,
                     FUN = var) > .Machine$double.eps]
    x <- scale(x)

    # If there any negative example, use the unlabeled
    plab <- which(y > 0)
    if (any(y < 0))
        nlab <- which(y < 0)
    else
        nlab <- which(y == 0)

    # An imbalance of 1:2 as maximum
    pnum <- min(length(plab), 2 * length(nlab))
    nnum <- min(length(nlab), 2 * length(plab))
    lab <- c(
        sample(
            x = plab,
            size = pnum,
            replace = FALSE
        ),
        sample(
            x = nlab,
            size = nnum,
            replace = FALSE
        )
    )

    variable <- apply(X = x[lab, ], MARGIN = 2, FUN = var) > .Machine$double.eps

    w <- rep(0, ncol(x))
    w[variable] <- attrEval(
        CLASS ~ .,
        data = data.frame( x[lab, variable], CLASS = as.factor(y[lab])),
        estimator = "ReliefFexpRank",
        kNearestExpRank = nn,
        numAttrProportionEqual = 0.0,
        numAttrProportionDifferent = 1.0
    )
    x <- x[, w > 0]
    w <- w[w > 0]
    w <- w / max(w)
    x <- sweep(x, MARGIN = 2, w, `*`)
    return(x)
}
