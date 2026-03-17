



# in example by W. Joel Schneider from 
# https://stackoverflow.com/questions/59997925/how-to-generate-multivariate-normal-data-in-r
# ?MASS::mvrnorm is faster than ?mvtnorm::rmvnorm

#' @title Expand Types of `Sigma` in \link[MASS]{mvrnorm}
#' 
#' @description
#' To accommodate more types of `Sigma` in the function \link[MASS]{mvrnorm}.
#' 
#' @param n \link[base]{integer} scalar, sample size
#' 
#' @param mu \link[base]{numeric} scalar or \link[base]{vector},
#' multivariate means \eqn{\mathbf{\mu}}'s
#' 
#' @param sd \link[base]{numeric} scalar or a \link[base]{vector}, standard deviation(s)
#' 
#' @param Sigma \link[base]{numeric} \link[stats]{var}iance-\link[stats]{cov}ariance \link[base]{matrix}, see function \link[MASS]{mvrnorm}
#' 
#' @param row.prefix,col.prefix (optional) \link[base]{character} scalars
#' 
#' @param ... additional parameter of the function \link[MASS]{mvrnorm}
#' 
#' @details
#' 
#' Argument of parameter `sd` could be
#' 
#' \describe{
#' 
#' \item{scalar}{`sd` is recycled to the \link[base]{length} of `mu`}
#' 
#' \item{\link[base]{vector}}{check that \link[base]{length} of `sd` and `mu` must be the same}
#' 
#' }
#' 
#' Then a \link[base]{diag}onal \link[base]{matrix} with \link[base]{vector} `sd^2` on the diagonal elements
#' is used as the \link[stats]{var}iance-\link[stats]{cov}ariance 
#' \link[base]{matrix} \eqn{\Sigma}
#' 
#' @returns 
#' The function [mvrnorm2()] returns a \link[base]{double} \link[base]{matrix}.
#' 
#' @note
#' The workhorse function \link[MASS]{mvrnorm} from package \CRANpkg{MASS} is faster than `?mvtnorm::rmvnorm`.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/nonS3/mvrnorm2.html}
#' 
#' @keywords internal
#' @importFrom MASS mvrnorm
#' @export
mvrnorm2 <- function(
    n, 
    mu, 
    sd, 
    Sigma = diag(x = sd^2, nrow = d, ncol = d),
    row.prefix, col.prefix,
    ...
) {
  
  d <- length(mu)
  
  if (!missing(sd)) {
    nsd <- length(sd)
    if (nsd == 1L) sd <- rep(sd, times = d)
    if (length(sd) != d) stop('`length(sd)` not same with length(mu)')
  }
  
  force(Sigma)
  
  z <- mvrnorm(n = n, mu = mu, Sigma = Sigma, ...) # matrix of `n`-by-`d`
  
  if (!missing(row.prefix)) {
    if (!is.character(row.prefix) || length(row.prefix) != 1L || is.na(row.prefix) || !nzchar(row.prefix)) stop('illegal `row.prefix`')
    rownames(z) <- paste(row.prefix, seq_len(n))
  }
  
  if (!missing(col.prefix)) {
    if (!is.character(col.prefix) || length(col.prefix) != 1L || is.na(col.prefix) || !nzchar(col.prefix)) stop('illegal `col.prefix`')
    colnames(z) <- paste(col.prefix, seq_len(ncol(z)))
  }
  
  return(z)
  
}
