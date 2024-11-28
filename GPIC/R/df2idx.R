#' Compute GPIC for Multiple Groups
#'
#' \code{df2idx} computes the index based on the number of prizes that several
#' groups obtained and the proportion of prizes in the pool.
#'
#' @param df a data frame with name of groups as the first column and number of
#'   prizes as remaining columns.
#' @param pool a vector of prize counts or proportions from the pool.
#' @param type the type of the above-mentioned \code{pool}, "n" for counts or
#'   "p" for proportions.
#'
#' @return \code{df2idx} returns a dataframe with name of groups as the first
#'   column and GPIC index as the second column.
#'
#' @export
#'
#' @examples
#' df2idx(vnomath)
#' df2idx(vnomath, c(61, 477, 836, 1007), "n")
#' df2idx(vnomath, c(0.026, 0.200, 0.351, 0.423), "p")
df2idx <- function(df, pool = NULL, type = NULL) {
    if (missing(pool)) {
        p <-  n2p(colSums(df[, -1], na.rm = T))
        data.frame(df[, 1], GPIC = apply(df[, -1], 1, vec2idx, p, "p"))
    } else {
        if (missing(type))
            stop("Type (i.e. counts, proportions) must be specified.")
        data.frame(df[, 1], GPIC = apply(df[, -1], 1, vec2idx, pool, type))
    }
}
