#' @title Diversity Indices
#' @description Calculate common diversity and entropy indices.
#' 
#' @param x vector or matrix of values (character, factor) representing a 
#'   class, from which proportions will be computed. If numeric, 
#'   values will be converted to proportions. If a matrix, indices
#'   will be computed for all columns.
#' @param type type of index to compute. See Details for descriptions.
#'   If \code{"renyi"} or \code{"hill"}, then \code{"q"} must be specified.
#' @param q order of Hill number (must be >= 0).
#' 
#' @note Available indices for \code{type} are: \describe{
#'   \item{richness}{the number of observed classes (non-\code{NA} and frequency > 0)}
#'   \item{effective.number}{exponent of Hill number of order 1}
#'   \item{shannon}{Shannon entropy}
#'   \item{simpson}{Simpson concentration}
#'   \item{gini.simpson}{Gini-Simpson index (= 1 - Simpson concentration)}
#'   \item{inv.simpson}{Inverse Simpson concentration}
#'   \item{unb.gini}{unbiased Gini-Simpson index with correction for small sample sizes}
#'   \item{eveness.simpson}{Simpson eveness}
#'   \item{eveness.pielou}{Pielou eveness}
#'   \item{renyi}{Renyi entropy}
#'   \item{hill}{Hill number}
#' }
#'   
#' @return if a vector is supplied for \code{x}, a single value for the chosen 
#'   type of index. If a matrix, a vector values for each column.
#'   
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' x <- sample(letters[1:4], 100, replace = TRUE, p = c(1, 2, 3, 4))
#' 
#' types <- c("richness", "effective.number", "shannon",
#'   "simpson", "inv.simpson", "gini.simpson", "unb.gini",
#'   "eveness.simpson", "eveness.pielou"
#' )
#' 
#' sapply(types, function(tp) diversity(x, type = tp))
#' 
#' # hill numbers with increasing order
#' order <- 0:5
#' hill.num <- sapply(order, function(q) diversity(x, type = "hill", q = q))
#' hill.num
#' plot(order, hill.num, type = "b")
#' 
#' # a matrix of frequencies
#' spp.freq <- cbind(
#'   sample(letters[1:4], 100, replace = TRUE, p = c(1, 1, 1, 4)),
#'   sample(letters[1:4], 100, replace = TRUE, p = c(4, 1, 1, 1)),
#'   sample(letters[1:4], 100, replace = TRUE, p = c(1, 1, 1, 1))
#')
#'
#'diversity(spp.freq, type = "eff")
#'
#' @export
#' 
diversity <- function(
    x, 
    type = c("effective.number", "richness", "shannon", "simpson", 
             "gini.simpson", "unb.gini", "eveness.simpson", "eveness.pielou",
             "inv.simpson", "renyi", "hill"),
    q = NULL
) {
  type <- match.arg(type)
  
  if(type %in% c("renyi", "hill")) {
    if(is.null(q)) stop("if type is 'renyi' or 'hill', then 'q' must be specified")
    if(!is.numeric(q)) stop("'q' must be numeric")
    if(length(q) != 1) stop("'q' must be a vector of length 1")
    if(q < 0) stop("'q' must be >= 0")
  }
  
  .hill <- function(p, q) {
    if(sum(p) == 0) return(0)
    if(q == 1) {
      exp(-sum(p * log(p), na.rm = TRUE)) 
    } else {
      sum(p ^ q, na.rm = TRUE) ^ (1 / (1 - q))
    }
  }
  
  .diversity <- function(x, type, q) {
    x <- x[!is.na(x)]
    p <- if(length(x) == 0) {
      0
    } else if(is.numeric(x)) {
      if(sum(x) == 0) 0 else x / sum(x) 
    } else {
      proportions(table(x))
    }
    p <- p[p > 0]
    
    switch(
      type,
      richness = .hill(p, 0),
      effective.number = .hill(p, 1),
      shannon = log(.hill(p, 1)),
      eveness.simpson = log(.hill(p, 1)) / .hill(p, 0),
      eveness.pielou = log(.hill(p, 1)) / log(.hill(p, 0)),
      simpson = 1 / .hill(p, 2),
      inv.simpson = .hill(p, 2),
      gini.simpson = 1 - (1 / .hill(p, 2)),
      unb.gini = length(x) * (1 - (1 / .hill(p, 2))) / (length(x) - 1),
      renyi = log(.hill(p, q)),
      hill = .hill(p, q)
    )
  }
  
  if(is.data.frame(x)) x <- as.matrix(x)
  if(is.matrix(x)) {
    apply(x, 2, .diversity, type = type, q = q)
  } else if(is.vector(x)) {
    .diversity(x, type = type, q = q)
  } else stop("'x' must be a matrix or vector")
}