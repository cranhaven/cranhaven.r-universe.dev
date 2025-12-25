#'@title prior.pois
#'
#'@description
#'This function computes the Pois prior proposed in Du, Kao and Kou (2015), which is used under the Poisson assumption with conjugate prior. The data is assumed to follow Poisson(\eqn{\lambda}), where \eqn{\lambda} is assumed to have Beta prior with shape parameters \eqn{\alpha} and \eqn{\beta}.
#'
#'@details
#'See Manual.pdf in "data" folder.
#'
#'@param
#'data.x Observed data in vector form where each element represents a single observation.
#'
#'@return
#'Vector for prior parameters in the order of (\eqn{\alpha, \beta})
#'
#'@references
#'Chao Du, Chu-Lan Michael Kao and S. C. Kou (2015), "Stepwise Signal Extraction via Marginal Likelihood." Forthcoming in Journal of American Statistical Association.
#'
#'@examples
#'n <- 20
#'
#'data.x <- rpois(n, 1)
#'data.x <- c(data.x, rpois(n, 10))
#'data.x <- c(data.x, rpois(n, 50))
#'data.x <- c(data.x, rpois(n, 20))
#'data.x <- c(data.x, rpois(n, 80))
#'
#'prior.pois(data.x)
#'
#'
#'@export
prior.pois <- function(data.x)
{
  temp.mean <- mean(data.x)
  if (temp.mean <= 0)
  {
    temp.mean <- 1
  }
  temp.var <- 0
  if (length(data.x)>1)
  {
    temp.var <- var(data.x)
  }
  if (temp.var<= 0)
  {
    temp.var <- 1
  }
  c(temp.mean/(2*temp.var),1/(2*temp.var))
}
