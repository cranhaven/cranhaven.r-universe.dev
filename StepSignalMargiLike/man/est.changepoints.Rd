\name{est.changepoints}
\alias{est.changepoints}
\title{est.changepoints}
\usage{
est.changepoints(data.x, model, prior, max.segs, logH, logMD)
}
\arguments{
  \item{data.x}{Observed data in vector or matrix form. When the data is in matrix form, each column should represent a single observation.}

  \item{model}{The specified distributional assumption. Currently we have implemented two arguments: "normal" (data follows one dimensional Normal distribution with unknown mean and variance) and "poisson" (data follows Poisson distribution with unknown intensity). A third argument "user" is also accepted, given that the prior and the log marginal likelihood function are specified in the parameter prior and logMD.}

  \item{prior}{The prespecified prior parameters in
  consistent with the form used in \code{logMD}.\cr For the
  proposed priors in DKK2015, use the corresponding
  \code{prior} function provided.}

  \item{max.segs}{(Opt.) The maximum number of segments allowed,
  which is the value M in DKK2015. Must be a positive
  integer greater then 1. If missing, the function would process using the
  algorihtm by Jackson et al.(2005).}

  \item{logH}{(Opt.) A Boolean algebra determine whether to
  report the log H matrix in DKK2015. Default is FALSE.}

  \item{logMD}{(Opt.) The log marginal likelihood function (which
  is the log of D function in DKK2015). The function must
  be in the form of \code{logMD(data.x, prior)}. }
}
\value{
If \code{logH} is FALSE, the function returns the set of
estimated change-points by the index of the data, where
each index is the end point of a segment. If the result
is no change-points, the function returns \code{NULL}. \cr
If \code{logH} is TRUE, then the function
returns a list with three components:
\code{changePTs} is the set of estimated change-points,
\code{log.H} is the log value for the H matrix used in the algorithm,
where \eqn{log.H(m,i) = log H(x1, x2, ..., xi | m)}, and \code{max.j}
records the \eqn{j} that maximizes the marginal likelihood in each step.
See the manual in data folder for more details.
}
\description{
This function estimates multiple change points using
marginal likelihood method proposed by Du, Kao and Kou
(2015), which we would denoted as DKK2015 afterward.
}
\details{
See Manual.pdf in "data" folder.
}
\examples{
library(StepSignalMargiLike)

n <- 5
data.x <- rnorm(n, 1, 1)
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 2,1))
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 1,1))

prior <- prior.norm.A(data.x)
max.segs <- 10

est.changepoints(data.x=data.x, model="normal", prior=prior)
est.changepoints(data.x=data.x, model="normal", prior=prior, max.segs=max.segs)
est.changepoints(data.x=data.x, model="normal", prior=prior, max.segs=max.segs,logH=TRUE)
}
\references{
Chao Du, Chu-Lan Michael Kao and S. C. Kou (2015),
"Stepwise Signal Extraction via Marginal Likelihood".
Forthcoming in Journal of American Statistical Association.
}

