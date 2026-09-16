#' An artificial dataset called MADELON
#'
#' An artificial dataset containing data points grouped in 32 clusters placed
#' on the vertices of a five dimensional hypercube and randomly labeled 0/1.
#'
#' The five dimensions constitute 5 informative features.
#' 15 linear combinations of those features are added to form a set of 20
#' (redundant) informative features.
#' There are 480 distractor features called 'probes' having no predictive
#' power.
#'
#' Included is the original training set with label -1 changed to 0.
#'
#' @format A list of two elements:
#' \describe{
#'   \item{data}{2000 by 500 matrix of 2000 objects with 500 features}
#'   \item{decision}{vector of 2000 decisions (labels 0/1)}
#'   \item{IG.2D}{example 2D IG computed using \code{ComputeMaxInfoGains}}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Madelon}
"madelon"
