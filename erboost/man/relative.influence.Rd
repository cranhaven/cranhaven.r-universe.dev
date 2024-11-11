\name{relative.influence}
\alias{relative.influence}
\alias{permutation.test.erboost}
\alias{erboost.loss}
\title{ Methods for estimating relative influence }
\description{
Helper functions for computing the relative influence of each variable in the erboost object.
}
\usage{
relative.influence(object, n.trees)
permutation.test.erboost(object, n.trees)
erboost.loss(y,f,w,offset,dist,baseline)
}
\arguments{
\item{object}{a \code{erboost} object created from an initial call to \code{\link{erboost}}.}
\item{n.trees}{ the number of trees to use for computations.}
\item{y,f,w,offset,dist,baseline}{For \code{erboost.loss}: These components are the
outcome, predicted value, observation weight, offset, distribution, and comparison
loss function, respectively.}
}
\details{
This is not intended for end-user use. These functions offer the different
methods for computing the relative influence in \code{\link{summary.erboost}}.
\code{erboost.loss} is a helper function for \code{permutation.test.erboost}.
}
\value{
Returns an unprocessed vector of estimated relative influences.
}
\references{
Yang, Y. and Zou, H. (2015), \dQuote{Nonparametric Multiple Expectile Regression via ER-Boost,} \emph{Journal of Statistical Computation and Simulation}, 84(1), 84-95.



G. Ridgeway (1999). \dQuote{The state of boosting,} \emph{Computing Science and
Statistics} 31:172-181.

\url{https://cran.r-project.org/package=gbm}\cr


J.H. Friedman (2001). "Greedy Function Approximation: A Gradient Boosting
Machine," Annals of Statistics 29(5):1189-1232.



}
\author{Yi Yang \email{yiyang@umn.edu} and Hui Zou \email{hzou@stat.umn.edu}}

\seealso{ \code{\link{summary.erboost}} }

\keyword{ hplot }
