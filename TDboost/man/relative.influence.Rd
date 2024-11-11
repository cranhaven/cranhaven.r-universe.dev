\name{relative.influence}
\alias{relative.influence}
\alias{permutation.test.TDboost}
\alias{TDboost.loss}
\title{ Methods for estimating relative influence }
\description{
Helper functions for computing the relative influence of each variable in the TDboost object.
}
\usage{
relative.influence(object, n.trees)
permutation.test.TDboost(object, n.trees)
TDboost.loss(y,f,w,offset,dist,baseline)
}
\arguments{
\item{object}{a \code{TDboost} object created from an initial call to \code{\link{TDboost}}.}
\item{n.trees}{ the number of trees to use for computations.}
\item{y,f,w,offset,dist,baseline}{For \code{TDboost.loss}: These components are the
outcome, predicted value, observation weight, offset, distribution, and comparison
loss function, respectively.}
}
\details{
This is not intended for end-user use. These functions offer the different
methods for computing the relative influence in \code{\link{summary.TDboost}}.
\code{TDboost.loss} is a helper function for \code{permutation.test.TDboost}.
}
\value{
Returns an unprocessed vector of estimated relative influences.
}
\references{
Yang, Y., Qian, W. and Zou, H. (2013), \dQuote{A Boosted Tweedie Compound Poisson Model for Insurance Premium} Preprint.



G. Ridgeway (1999). \dQuote{The state of boosting,} \emph{Computing Science and
Statistics} 31:172-181.




J.H. Friedman (2001). "Greedy Function Approximation: A Gradient Boosting
Machine," Annals of Statistics 29(5):1189-1232.

}
\author{Yi Yang \email{yi.yang6@mcgill.ca}, Wei Qian \email{wxqsma@rit.edu} and Hui Zou \email{hzou@stat.umn.edu}}

\seealso{ \code{\link{summary.TDboost}} }

\keyword{ hplot }
