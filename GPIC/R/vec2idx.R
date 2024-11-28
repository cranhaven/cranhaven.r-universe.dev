#' Compute GPIC for Single Group
#'
#' \code{vec2idx} computes the index based on the number of prizes that a group
#' obtained and the proportion of prizes in the pool.
#'
#' @param x a vector of prize counts from a single group.
#' @param pool a vector of prize counts or proportions from the pool.
#' @param type the type of the above-mentioned \code{pool}, "n" for counts or
#'   "p" for proportions.
#'
#' @return \code{vec2idx} returns a numeric that is the GPIC index.
#'
#' @export
#'
#' @examples
#' vec2idx(c(3, 19, 34, 22), c(61, 477, 836, 1007), "n")
#' vec2idx(c(3, 19, 34, 22), c(0.026, 0.200, 0.351, 0.423), "p")
vec2idx <- function(x, pool, type) {
    if (!type %in% c("n", "p"))
        stop('Only "n" and "p" are valid.')
    if (length(x) != length(pool))
        stop("Two vectors must be the same length.")
    if (type == "n") {
        p <- n2p(pool)
    }
    if (type == "p") {
        if (sum(pool) != 1)
            warning("Proportions does not sum up to 1.")
        p <- pool
    }
    sum(x * log(p, min(p)), na.rm = T)
}
