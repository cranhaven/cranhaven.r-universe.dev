#'@title prior.norm.B
#'
#'@description
#'This function computes the Norm-B prior proposed in Du, Kao and Kou (2015), which is used under conjugate normal assumption. The variance \eqn{\sigma^2} is assumed to be drawn from an inverse Gamma distribution with shape parameter \eqn{\nu0} and scale parameter \eqn{\sigma0^2}, while mean is assumed to be drawn from a normal distribution with mean \eqn{\mu0} and variance \eqn{\sigma^2/\kappa0}.
#'
#'@details
#'See Manual.pdf in "data" folder.
#'
#'@param
#'data.x Observed data in vector form where each element represents a single observation.
#'
#'@return
#'Vector for prior parameters in the order of (\eqn{\mu0, \kappa0, \nu0, \sigma0^2})
#'
#'@references
#'Chao Du, Chu-Lan Michael Kao and S. C. Kou (2015), "Stepwise Signal Extraction via Marginal Likelihood." Forthcoming in Journal of American Statistical Association.
#'
#'@examples
#'library(StepSignalMargiLike)
#'
#'n <- 5
#'data.x <- rnorm(n, 1, 1)
#'data.x <- c(data.x, rnorm(n, 10,1))
#'data.x <- c(data.x, rnorm(n, 2,1))
#'data.x <- c(data.x, rnorm(n, 10,1))
#'data.x <- c(data.x, rnorm(n, 1,1))
#'
#'prior.norm.B(data.x)
#'
#'@export
prior.norm.B <- function(data.x)
{
  temp.var <- 0
  if (length(data.x)>1)
  {
    temp.var <- var(data.x)
  }
  if (temp.var<= 0)
  {
    temp.var <- 1
  }
  c(mean(data.x), 0.5, 3, 2.5*temp.var)
}