#' Generates clustering result
#' A clustering result is generated from a membership tensor.
#' @param object WormTensor object with a membership tensor
#' @param num.iter The upper limit of iterations (Default value is 30)
#' @param thr The lower limit of relative change in estimates
#' (Default value is 1E-10)
#' @param verbose Control message
#' @param algorithm Clustering methods
#' @return WormTensor object with a clustering result added
#' @examples
#' \donttest{
#'     # Pipe Operation
#'     worm_download("Euclid", qc = "WARN")$Ds |>
#'         as_worm_tensor() |>
#'             worm_membership(k = 6) -> object
#'     worm_clustering(object, verbose = TRUE) -> ob_mcmi
#'     worm_clustering(object, algorithm = "OINDSCAL", verbose = TRUE) -> ob_oind
#'     worm_clustering(object, algorithm = "CSPA", verbose = TRUE) -> ob_cspa
#' }
#' @import rTensor
#' @importFrom stats as.dist
#' @importFrom stats cutree
#' @importFrom stats dist
#' @importFrom stats hclust
#' @export
setMethod(
    "worm_clustering",
    signature(object = "WormTensor"),
    function(object, algorithm, num.iter, thr, verbose) {
        # Argument Check
        .check_worm_clustering(object, num.iter, thr, verbose)
        algorithm <- match.arg(algorithm)
        # Clustering
        res <- .clusteringFunc[[algorithm]](object, num.iter, thr, verbose)
        if (algorithm == "MCMI") {
            object@clustering <- res$clustering
            object@factor <- res$factor
            object@weight <- res$weight
        }
        if (algorithm == "OINDSCAL") {
            object@clustering <- res$clustering
            object@factor <- res$factor
        }
        if (algorithm == "CSPA") {
            object@clustering <- res$clustering
            object@consensus <- res$consensus
        }
        # Output
        object@clustering_algorithm <- algorithm
        object
    }
)

.check_worm_clustering <- function(object, num.iter, thr, verbose) {
    # Backword Check
    if (length(dim(object@membership_tensor)) == 0) {
        msg <- paste0("Perform worm_membership() first.")
        message(msg)
    }
    # Argument Check
    stopifnot(is.numeric(num.iter))
    if (num.iter <= 0 || !.is_integer(num.iter)) {
        stop("Specify num.iter as a positive integer.")
    }
    stopifnot(is.numeric(thr))
    if (thr <= 0) {
        stop("Specify thr as a positive float number.")
    }
    stopifnot(is.logical(verbose))
}

.is_integer <- function(values) {
    all(values %% 1 == 0)
}

######## MCMI #######
.MCMI <- function(object, num.iter, thr, verbose) {
    # Initialization
    A <- object@membership_tensor
    A1 <- rs_unfold(A, m = 1)@data
    U <- svd(A1)$u[, seq(object@k)]
    # Iteration
    iter <- 1
    RecError <- c()
    RelChange <- c()
    RecError[iter] <- 10^2
    RelChange[iter] <- 10^2
    while (RelChange[iter] >= thr && iter <= num.iter) {
        # Step1
        A3 <- rs_unfold(A, m = 3)@data
        W <- as.matrix(svd(A3 %*% kronecker(U, U))$u[, 1])
        # Step2
        S_ <- array(0, dim = dim(A)[1:2])
        for (i in seq_len(dim(A)[3])) {
            S_ <- S_ + W[i] * A@data[, , i]
        }
        # Step3
        U <- svd(S_)$u[, seq(object@k)]
        # Update
        iter <- iter + 1
        A |>
            ttm(t(U), m = 1) |>
            ttm(t(U), m = 2) |>
            ttm(t(W), m = 3) -> G
        G |>
            ttm(U, m = 1) |>
            ttm(U, m = 2) |>
            ttm(W, m = 3) -> A_bar
        RecError[iter] <- .recErrorTensor(A, A_bar)
        RelChange[iter] <- abs(RecError[iter - 1] - RecError[iter]) /
            RecError[iter]
        if (verbose) {
            cat(paste0(
                iter - 1, " / ", num.iter,
                " |Previous Error - Error| / Error = ", RelChange[iter], "\n"
            ))
        }
    }
    # Clustering
    dist_mat <- dist(U)
    # Clustering
    dist_mat |>
        hclust(method = "ward.D2") |>
        cutree(object@k) -> out
    # Output
    list(clustering = out, factor = U, weight = as.vector(W))
}

.recErrorTensor <- function(A, A_bar) {
    sqrt(sum((A@data - A_bar@data)^2))
}

######## OINDSCAL #######
.OINDSCAL <- function(object, num.iter, thr, verbose) {
    # Initialization
    S <- object@membership_tensor
    X <- svd(k_unfold(S, m = 1)@data)$u[, seq(object@k)]
    D <- .calcD(S, X)
    G <- .calcG(S, X, D)
    # Iteration
    iter <- 1
    RecError <- c()
    RelChange <- c()
    RecError[iter] <- 10^2
    RelChange[iter] <- 10^2
    while (RelChange[iter] >= thr && iter <= num.iter) {
        X <- .calcX(G)
        D <- .calcD(S, X)
        G <- .calcG(S, X, D)
        iter <- iter + 1
        S_bar <- .recMatrices(X, D)
        RecError[iter] <- .recErrorTensorList(S, S_bar)
        RelChange[iter] <- abs(
            RecError[iter - 1] - RecError[iter]
        ) / RecError[iter]
        if (verbose) {
            cat(paste0(
                iter - 1, " / ", num.iter,
                " |Previous Error - Error| / Error = ", RelChange[iter], "\n"
            ))
        }
    }
    # Clustering
    dist_mat <- dist(X)
    dist_mat |>
        hclust(method = "ward.D2") |>
        cutree(object@k) -> out
    # Output
    names(out) <- dimnames(object@membership_tensor@data)[[1]]
    list(clustering = out, factor = X)
}

.calcD <- function(S, X) {
    lapply(seq(dim(S)[3]), function(s) {
        Ss <- S@data[, , s]
        out <- matrix(0, nrow = ncol(X), ncol = ncol(X))
        diag(out) <- diag(t(X) %*% Ss %*% X)
        out[which(out < 0)] <- 0
        out
    })
}

.calcG <- function(S, X, D) {
    out <- lapply(seq(dim(S)[3]), function(s) {
        S@data[, , s] %*% X %*% D[[s]]
    })
    Reduce("+", out)
}

.calcX <- function(G) {
    res <- svd(G)
    res$u %*% t(res$v)
}

.recMatrices <- function(X, D) {
    lapply(seq_along(D), function(s) {
        X %*% D[[s]] %*% t(X)
    })
}

.recErrorTensorList <- function(S, S_bar) {
    out <- lapply(seq(dim(S)[3]), function(s) {
        S@data[, , s] - S_bar[[s]]
    })
    sqrt(sum(unlist(out)^2))
}

######## CSPA #######
.CSPA <- function(object, num.iter, thr, verbose) {
    # Consensus matrix
    cons_mat <- modeSum(object@membership_tensor, m = 3, drop = TRUE)@data
    cons_mat <- cons_mat / length(object@dist_matrices)
    dist_mat <- as.dist(1 - cons_mat)
    dist_mat |>
        hclust(method = "ward.D2") |>
        cutree(object@k) -> out
    # Output
    names(out) <- dimnames(object@membership_tensor@data)[[1]]
    list(consensus = cons_mat, clustering = out)
}

.clusteringFunc <- list(
    "CSPA" = .CSPA,
    "OINDSCAL" = .OINDSCAL,
    "MCMI" = .MCMI
)
