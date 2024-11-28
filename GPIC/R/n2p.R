#' Calculate Proportions
#'
#' \code{n2p} converts a vector of counts to a vector of proportions.
#'
#' @param n a vector of counts.
#'
#' @return \code{n2p} returns a vector of proportions.
#'
#' @export
#'
#' @examples
#' n2p(c(61, 477, 836, 1007))
n2p <- function(n) {
    n / sum(n)
}
