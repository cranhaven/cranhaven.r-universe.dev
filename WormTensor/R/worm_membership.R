#' Generates membership tensor
#' A membership tensor is generated from distance matrices.
#' @param object WormTensor object with distance matrices
#' @param k Assumed number of clusters
#' @return WormTensor object with membership tensor added
#' @examples
#' \donttest{
#'     # Pipe Operation
#'     worm_download("mSBD", qc = "PASS")$Ds |>
#'         as_worm_tensor() -> object
#'     # k=6
#'     worm_membership(object, k = 6) -> object_k6
#' }
#' @import rTensor
#' @importFrom stats cutree
#' @importFrom stats hclust
#' @export
setMethod(
    "worm_membership",
    signature(object = "WormTensor"),
    function(object, k) {
        # Setting
        I <- length(object@union_cellnames)
        M <- length(object@dist_matrices)
        # Argment Check
        .check_worm_membership(object, k, I, M)
        # Clustering result for each animal
        Cs <- lapply(object@dist_matrices,
            function(d, k) {
                d |>
                    hclust(method = "ward.D2") |>
                    cutree(k)
            },
            k = k
        )
        # Cs → Indicator matrices
        Hs <- lapply(Cs, function(x) {
            out <- matrix(0,
                nrow = length(x),
                ncol = length(unique(x))
            )
            for (i in seq_along(x)) {
                out[i, x[i]] <- 1
            }
            rownames(out) <- names(x)
            colnames(out) <- paste0("Cluster", seq(ncol(out)))
            out
        })
        # Register the result as a tensor
        arr <- array(0, dim = c(I, I, M))
        dimnames(arr) <- list(
            object@union_cellnames,
            object@union_cellnames,
            paste0("Animal", seq(M))
        )
        for (m in seq(M)) {
            idx <- .search_position(object@union_cellnames, rownames(Hs[[m]]))
            arr[idx, idx, m] <- Hs[[m]] %*% t(Hs[[m]])
        }
        # Output
        object@membership_tensor <- as.tensor(arr)
        object@k <- k
        object
    }
)

.check_worm_membership <- function(object, k, I, M) {
    # Backword Check
    if (length(object@dist_matrices) == 0) {
        stop("Perform as_worm_tensor() first.")
    }
    # Dimension Check
    stopifnot(is.numeric(k))
    stopifnot(k >= 2)
    stopifnot(k <= I^2)
    stopifnot(k <= I * M)
}

.search_position <- function(union_cellnames, mth_cellnames) {
    mth_cellnames |>
        lapply(function(x) {
            which(x == union_cellnames)
        }) |>
        unlist()
}
