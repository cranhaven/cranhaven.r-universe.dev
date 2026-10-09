#' Calculate adjacency matrices and fragility matrix from iEEG recording
#'
#' The function calculates the neural fragility column
#' from an adjacency matrix in each time window
#'
#' @source Recreation of the method described in
#' Li A, Huynh C, Fitzgerald Z, Cajigas I, Brusko D, Jagid J, et al.
#' Neural fragility as an EEG marker of the seizure onset zone.
#'  Nat Neurosci. 2021 Oct;24(10):1465–74
#' (\href{https://pubmed.ncbi.nlm.nih.gov/34354282/}{pubmed}).
#' We have found solutions to fill up missing details in the paper method description
#'
#' @param epoch Matrix or Epoch object. iEEG data matrix or Epoch object. If matrix, the row names are the electrode names and the column names are the time points. For a matrix input, the sampling rate is assumed to be 1 Hz and the start time is 0.
#' @param window Integer. The number of time points to use in each window
#' @param step Integer. The number of time points to move the window each time
#' @param lambda Numeric. The lambda value for regularization to use in the ridge regression.
#' If NULL, the lambda will be chosen automatically
#' ensuring that ensuring that the adjacent matrix is stable (see details)
#' @param nSearch Integer. Number of instable eigenvalues with norm=1 to search for the minimum norm perturbation. This parameter is used only when the lambda is NULL
#' @param progress Logical. If TRUE, print progress information. If `parallel` is TRUE, this option only support the `doSNOW` backend.
#' @param parallel Logical. If TRUE, use parallel computing.
#' Users must register a parallel backend with the foreach package
#'
#'
#' @return A Fragility object
#'
#' @examples
#' ## A dummy example with 5 electrodes and 20 time points
#' data <- matrix(rnorm(100), nrow = 5)
#' ## create an Epoch object
#' epoch <- Epoch(data, startTime = 0, samplingRate = 1)
#' windowNum <- 10
#' step <- 5
#' lambda <- 0.1
#' calcAdjFrag(
#'     epoch = epoch, window = windowNum,
#'     step = step, lambda = lambda, progress = TRUE
#' )
#'
#' ## A more realistic example with parallel computing
#' \donttest{
#' if (requireNamespace("doSNOW")) {
#'     ## Register a SNOW backend with 4 workers
#'     library(parallel)
#'     library(doSNOW)
#'     cl <- makeCluster(4, type = "SOCK")
#'     registerDoSNOW(cl)
#'
#'     data("pt01EcoG")
#'     window <- 250
#'     step <- 125
#'     title <- "PT01 seizure 1"
#'     calcAdjFrag(
#'         epoch = pt01EcoG, window = window,
#'         step = step, parallel = TRUE, progress = TRUE
#'     )
#'
#'     ## stop the parallel backend
#'     stopCluster(cl)
#' }
#' }
#'
#' @details
#' 1/ For each time window i, a discrete stable Linear time system
#' (adjacency matrix) is computed named \eqn{A_i}
#' such that
#' \eqn{A_i x(t) = x(t+1)}. The 'lambda' option is the regularization parameter
#' for the ridge regression.
#' `lambda=NULL`(default) will find a lambda value that ensures
#' the stability of the estimated \eqn{A_i}.
#'
#' 2/For each stable estimated \eqn{A_i}, the minimum norm perturbation \eqn{\Gamma_{ik}} (k index of the electrodes)
#' for column perturbation is computed.
#' Each column is normalized \eqn{\frac{max(\Gamma_{i})-\Gamma_{ik}}{max(\Gamma_i)}}
#'
#' @export
calcAdjFrag <- function(epoch, window, step, lambda = NULL, nSearch = 100L, progress = FALSE, parallel = FALSE) {
    ## check the input types
    stopifnot(is.matrix(epoch) | is(epoch, "Epoch"))
    stopifnot(isWholeNumber(window))
    stopifnot(isWholeNumber(step))
    stopifnot(step > 0)
    stopifnot(is.null(lambda) | is.numeric(lambda))


    if (!is(epoch, "Epoch")) {
        epoch <- Epoch(epoch, startTime = 0, samplingRate = 1)
    }
    elecNum <- nrow(epoch)
    timeNum <- ncol(epoch)
    elecNames <- rownames(epoch)
    timePoints <- coltimes(epoch)
    dataMat <- tblData(epoch)

    ## The input matrix must have at least window rows
    stopifnot(timeNum >= window)

    dataMat <- standardizeIEEG(dataMat)
    # Number/sequence of steps
    nWindows <- floor((timeNum - window) / step) + 1L
    windows <- seq_len(nWindows)
    # Pre-allocate output

    # dimension
    dm <- c(elecNum, elecNum, nWindows)
    # dimension names
    dmn <- list(Electrode = elecNames, Step = windows)
    # dimension names for adjacency matrix
    dmnA <- list(Electrode1 = elecNames, Electrode2 = elecNames, Step = windows)

    # Pre-allocate output
    A <- array(.0, dim = dm, dimnames = dmnA)
    R2 <- array(.0, dim = dm[-1], dimnames = dmn)
    f <- fR <- R2
    lbd <- rep(0, nWindows) |> setNames(windows)

    ## switch between parallel and sequential computing
    if (parallel) {
        `%run%` <- foreach::`%dopar%`
    } else {
        `%run%` <- foreach::`%do%`
    }

    ## initialize the progress bar
    if (progress) {
        pb <- progress_bar$new(
            format = "Step = :current/:total [:bar] :percent in :elapsed | eta: :eta",
            total = nWindows,
            width = 60
        )

        progress <- function(n) {
            pb$tick()
        }
        opts <- list(progress = progress)
        on.exit(pb$terminate())
    } else {
        opts <- list()

        progress <- function(n) {
            NULL
        }
    }

    ## Initial data for data aggregation
    init <- list(A = A, R2 = R2, f = f, lbd = lbd)
    foreach(
        iw = windows,
        .combine = .combine,
        .init = init,
        .inorder = FALSE,
        .options.snow = opts
    ) %run% {
        progress(iw)
        ## Those are necessary to clear R check issue
        iw <- get("iw")
        .ridgeSearch <- utils::getFromNamespace("ridgeSearch", "EZFragility")
        .ridgeR2 <- utils::getFromNamespace("ridgeR2", "EZFragility")
        .fragilityRow <- utils::getFromNamespace("fragilityRow", "EZFragility")

        ## slice indices
        si <- (iw - 1L) * step + seq_len(window - 1L)
        xt <- dataMat[, si, drop = FALSE]
        xtp1 <- dataMat[, si + 1L, drop = FALSE]

        adjMatrix <- .ridgeSearch(xt, xtp1, lambda)
        R2Column <- .ridgeR2(xt, xtp1, adjMatrix)
        fColumn <- .fragilityRow(adjMatrix, nSearch)

        list(
            iw = iw,
            adjMatrix = adjMatrix,
            R2Column = R2Column,
            fColumn = fColumn
        )
    } -> results

    ## unpack the results
    A <- results$A
    R2 <- results$R2
    f <- results$f
    lbd <- results$lbd

    ## column rank of fragility matrix
    fR <- apply(f, 2, rank) / elecNum

    ## start time point/indices for each partition
    startTimes <- (seq_len(nWindows) - 1L) * step + 1L
    if (!is.null(timePoints)) {
        startTimes <- timePoints[startTimes]
    }

    Fragility(
        ieegts = dataMat,
        adj = A,
        R2 = R2,
        frag = f,
        frag_ranked = fR,
        lambdas = lbd,
        startTimes = startTimes,
        electrodes = elecNames
    )
}

.combine <- function(results, x) {
    iw <- x$iw
    adjMatrix <- x$adjMatrix
    R2Column <- x$R2Column
    fColumn <- x$fColumn

    results$A[, , iw] <- adjMatrix
    results$R2[, iw] <- R2Column
    results$f[, iw] <- fColumn
    results$lbd[[iw]] <- attr(adjMatrix, "lambda")
    results
}

#' Find Seizure Onset Zone
#' 
#' The function estimates the seizure onset zone (SOZ). For each row, it calculates the maximum, minimum, or mean of row. The rows with the highest values are considered as the SOZ.
#'
#' @param x Fragility object
#' @param method Character. The method to use to find the onset zone.
#' Must be one of 'max', 'min', or "mean"
#' @param proportion Numeric. The proportion of electrodes to consider as the onset zone.
#' The electrode number will be rounded to the nearest integer.
#' @param ... Additional arguments
#'
#' @return A vector of electrode names, or indices if the electrode names are NULL
#' @export
estimateSOZ <- function(x, method = c("mean", "median", "max", "min"), proportion = 0.1, ...) {
    method <- match.arg(method)

    stopifnot(is(x, "Fragility"))

    fragMat <- x$frag
    elCnt <- nrow(fragMat)
    nSOZ <- ceiling(elCnt * proportion)
    stopifnot(nSOZ > 0 & nSOZ <= elCnt)

    if (method == "max") {
        stat <- apply(fragMat, 1, max)
    } else if (method == "min") {
        stat <- apply(fragMat, 1, min)
    } else if (method == "mean") {
        stat <- apply(fragMat, 1, mean)
    } else if (method == "median") {
        stat <- apply(fragMat, 1, median)
    } else {
        stop("Unknown method: ", method)
    }

    sozIndex <- order(stat, decreasing = TRUE)[seq_len(nSOZ)]
    if (!is.null(x$electrodes)) {
        sozIndex <- x$electrodes[sozIndex]
    }

    sozIndex
}


standardizeIEEG <- function(data) {
    scaling <- 10^floor(log10(max(data)))
    plotData <- data / scaling
}
