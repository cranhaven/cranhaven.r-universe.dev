# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' Calculate the diversity index
#'
#' This function calculates the entropy of a system with discrete states
#' @param x numeric vector, observed probabilities of the classes
#' @param prior numeric vector, the prior probabilities of the classes
#' @keywords entropy
#' @returns the entropy or diversity measure
#' @export
#' @examples
#' x <- c(0.4, 0.6)
#' diversity(x)
diversity <- function(x, prior = NULL) {
  if (min(x) < 0) {return(-1);} # the log will fail for 0
  # if the numbers are higher than 1, then not probabilities but
    # populations are given, so we rescale to probabilities:
  if (sum(x) != 1) {x <- x / sum(x)}
  N <- length(x)
  xx <- x
  if(!is.null(prior)) {
    if (min(prior) < 0) {return(-1);} # the log will fail for 0
    if (sum(prior) != 1) {prior <- prior / sum(prior)}
    if (max(prior) <= 0.7) {
     for (i in (1:N)) {
        #a <- (1 - 1 / (N * prior[i])) / (1 - prior[i])
        #b <- (1 - N * prior[i]^2) / (N * prior[i] * (1 - prior[i]))

        a <- (prior[i] - 1/N) / (prior[i] - prior[i]^2)
        b <- (1/N - prior[i]^2) / (prior[i] - prior[i]^2)
        xx[i] <- a * x[i]^2 + b * x[i]
        }
      } else {# This fails if one of the prior[i] is higher than 0.7
              # some of the xx[i] will then be negative.
              # So, in this case, we try linear (asymmetric and not C2)
      for (i in (1:N)) {
        if (x[i] <= prior[i]) {
          a <- 1 / (N * prior[i])
          b <- 0
        } else {
          a <- (1 - 1/N) / (1 - prior[i])
          b <- (1/N - prior[i]) / (1 - prior[i])
        }
        xx[i] <- a * x[i] + b
      } #for
    } # if quadratic failed
  } # if prior was provided
  f <- function(xxx) {
    if (is.na(xxx)) return(0)  #TODO: this needs to be investigated further
    if(xxx == 0) {
       return(0)  # if x = 0 then log(x) is NA -> return the limit for x.log(x)
     } else {
       return(xxx * log(xxx))
     }
    }
  x1 <- mapply(FUN = f, xx)
  - sum(x1) / log(N)
  } #function
