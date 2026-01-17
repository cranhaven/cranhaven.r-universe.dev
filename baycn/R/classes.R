#' baycn class
#'
#' @slot burnIn The percentage of MCMC iterations that will be discarded from
#' the beginning of the chain.
#'
#' @slot chain A matrix where the rows contain the vector of edge states for the
#' accepted graph.
#'
#' @slot decimal A vector of decimal numbers. Each element in the vector is the
#' decimal of the accepted graph.
#'
#' @slot iterations The number of iterations for which the Metropolis-Hastings
#' algorithm is run.
#'
#' @slot posteriorES A matrix of posterior probabilities for all three edge
#' states for each edge in the network.
#'
#' @slot posteriorPM A posterior probability adjacency matrix.
#'
#' @slot likelihood A vector of log likelihood values. Each element in the
#' vector is the log likelihood of the accepted graph.
#'
#' @slot stepSize The number of iterations discarded between each iteration that
#' is kept.
#'
#' @slot time The runtime of the Metropolis-Hastings algorithm in seconds.
#'
#' @exportClass baycn
#'
setClass(Class = 'baycn',
         slots = c(burnIn = 'numeric',
                   chain = 'matrix',
                   decimal = 'numeric',
                   iterations = 'numeric',
                   posteriorES = 'data.frame',
                   posteriorPM = 'matrix',
                   likelihood = 'numeric',
                   stepSize = 'numeric',
                   time = 'numeric'))
