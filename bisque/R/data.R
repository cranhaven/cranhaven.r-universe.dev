#' Data from a capture-recapture study of fur seal pups
#'
#' These data are used in the book "Computational Statistics" 
#' by G.H. Givens and J.A. Hoeting (2013).  They are discussed in Chapter 7, 
#' Examples 7.2,7.3,7.8, and Exercise 7.2.
#' 
#' As described by the authors:
#' 
#' Source: Richard Barker, University of Otago, New Zealand
#'
#' Description: Data from a capture-recapture study conducted on the
#' Otago Penninsula, South Island, New Zealand.  Fur seal pups were
#' marked and released during 7 census attempts in one season.  The
#' population is assumed closed.  For each census attempt, the number of
#' pups captured and the number of these captures corresponding to pups
#' never previously caught are recorded.
#'
#' @format A data.frame with variables:
#' \describe{
#'   \item{i}{The census attempt}
#'   \item{c}{Number of pups captured in census attempt}
#'   \item{m}{Number of newly captured pups}
#' }
#' 
#' @usage data(furseals)
#' 
#' @source \url{https://www.stat.colostate.edu/computationalstatistics/}
#' @source \url{https://www.stat.colostate.edu/computationalstatistics/datasets.zip}
#' 
#' @examples
#' 
#' data("furseals")
#' str(furseals)
#' 
"furseals"