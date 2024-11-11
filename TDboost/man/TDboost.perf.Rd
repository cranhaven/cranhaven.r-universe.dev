\name{TDboost.perf}
\alias{TDboost.perf}
\title{TDboost performance}
\description{
Estimates the optimal number of boosting iterations for a \code{TDboost} object and
optionally plots various performance measures
}
\usage{
TDboost.perf(object, 
         plot.it = TRUE, 
         oobag.curve = FALSE, 
         overlay = TRUE, 
         method)
}
\arguments{
\item{object}{a \code{\link{TDboost.object}} created from an initial call to 
\code{\link{TDboost}}.}
\item{plot.it}{an indicator of whether or not to plot the performance measures.
Setting \code{plot.it=TRUE} creates two plots. The first plot plots 
\code{object$train.error} (in black) and \code{object$valid.error} (in red) 
versus the iteration number. The scale of the error measurement, shown on the 
left vertical axis, depends on the \code{distribution} argument used in the 
initial call to \code{\link{TDboost}}.}
\item{oobag.curve}{indicates whether to plot the out-of-bag performance measures
in a second plot.}
\item{overlay}{if TRUE and oobag.curve=TRUE then a right y-axis is added to the 
training and test error plot and the estimated cumulative improvement in the loss 
function is plotted versus the iteration number.}
\item{method}{indicate the method used to estimate the optimal number
of boosting iterations. \code{method="OOB"} computes the out-of-bag
estimate and \code{method="test"} uses the test (or validation) dataset 
to compute an out-of-sample estimate. \code{method="cv"} extracts the 
optimal number of iterations using cross-validation if \code{TDboost} was called
with \code{cv.folds}>1}
}
\value{
\code{TDboost.perf} returns the estimated optimal number of iterations. The method 
of computation depends on the \code{method} argument.}
\references{
Yang, Y., Qian, W. and Zou, H. (2013), \dQuote{A Boosted Tweedie Compound Poisson Model for Insurance Premium} Preprint.

G. Ridgeway (1999). \dQuote{The state of boosting,} \emph{Computing Science and
Statistics} 31:172-181.

 G. Ridgeway (2003). "A note on out-of-bag estimation for estimating the optimal
number of boosting iterations," Working paper.
}
\author{Yi Yang \email{yi.yang6@mcgill.ca}, Wei Qian \email{wxqsma@rit.edu} and Hui Zou \email{hzou@stat.umn.edu}}
\seealso{\code{\link{TDboost}}, \code{\link{TDboost.object}}}

\keyword{nonlinear}
\keyword{survival}
\keyword{nonparametric}
\keyword{tree}
