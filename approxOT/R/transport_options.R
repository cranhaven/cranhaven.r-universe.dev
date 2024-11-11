#' Function returning supported optimal transportation methods.
#'
#' @return Returns a vector of supported transport methods
#' @export
#'
#' @details The currently supported methods are
#' \itemize{
#' \item exact, networkflow: Utilize the networkflow algorithm to solve the exact optimal transport problem
#' \item shortsimplex: Use the shortsimplex algorithm to solve the exact optimal transport problem
#' \item sinkhorn: Use Sinkhorn's algorithm to solve the approximate optimal transport problem
#' \item sinkhorn_log: Use Sinkhorn's algorithm on a log-scale for added stability to solve the approximate optimal transport problem
#' \item greenkhorn: Use the Greenkhorn algorithm to solve the approximate optimal transport problem
#' \item hilbert: Use hilbert sorting to perform approximate optimal transport
#' \item rank: use the average covariate ranks to perform approximate optimal transport
#' \item univariate: Use appropriate optimal transport methods for univariate data
#' \item swapping: Utilize the swapping algorithm to perform approximate optimal transport
#' \item sliced: Use the sliced optimal transport distance
#' }
transport_options <- function() {
  return(c("exact", "networkflow","shortsimplex",
           "sinkhorn", "sinkhorn_log",
           "greenkhorn",
           # "randkhorn", "gandkhorn",
           "hilbert", "rank", "univariate",
           "univariate.approximation.pwr",
           "swapping", "sliced"))
}