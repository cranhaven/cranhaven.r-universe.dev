\name{totalIndex}
\alias{totalIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimation of main index indices
}
\description{Estimation of the unscaled total Sobol index of all main indices by method Liu & Owen (superset importance of main indices).
}
\usage{
totalIndex(f.mat, d, q = NULL, q.arg = NULL, n.mc, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f.mat}{vectorized function of which indices shall be estimated
%%     ~~Describe \code{f.mat} here~~
}
  \item{d}{integer, number of input factors (vertices)
}
  \item{q}{a vector of character strings of quantile functions corresponding to the factors distributions, it can be a single character string meaning same distribution for all, if not specified \code{"qunif"} is taken
}
  \item{q.arg}{a list of lists of quantile functions parameters of the distributions in \code{q}, it can be a single list meaning same parameters for all, if not specified the default values of the respective distributions are taken
}
  \item{n.mc}{number of Monte Carlo evaluations
}
  \item{...}{additional arguments to be passed to the function \code{f.mat}
}
}
\value{
A vector containing the unscaled total Sobol indices}
\references{
Liu, R.; Owen, A.B. (2006) Estimating mean dimensionality of analysis of variance decompositions, \emph{Journal of the American Statistical Association}, \bold{101} 474, 712-721.
}
\author{
J. Fruth
}
\seealso{
\code{\link{estimateGraph}}
}
\examples{
totalIndex(f.mat=ishigami.fun, d=3, q.arg=list(min=-pi,max=pi), n.mc=10000)
totalIndex(f.mat=sobol.fun, d=8, q.arg=list(min=0,max=1), n.mc=10000)
}
