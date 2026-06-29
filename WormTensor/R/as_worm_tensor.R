#' Generates WormTensor object
#' A WormTensor object is generated from distance matrices.
#' @param Ds A list containing distance matrices
#' @return An object containing distance matrices and metadata
#' @examples
#' \donttest{
#'     worm_download("mSBD", qc = "PASS")$Ds |> as_worm_tensor() -> object
#' }
#' @import rTensor
#' @importFrom methods new
#' @importFrom methods is
#' @export
as_worm_tensor <- function(Ds) {
    .check_as_worm_tensor(Ds)
    object <- new("WormTensor")
    object@dist_matrices <- Ds
    object@n_animals <- length(Ds)
    object@union_cellnames <- .union_cellnames(Ds)
    object@n_union_cells <- length(object@union_cellnames)
    object
}

.union_cellnames <- function(Ds) {
    Ds |>
        lapply(function(x) {
            attr(x, "Labels")
        }) |>
        unlist() |>
        unique() |>
        sort() -> res
    res
}

.check_as_worm_tensor <- function(Ds) {
    Ds |>
        lapply(function(x) {
            stopifnot(is(x, "dist"))
        })
}
