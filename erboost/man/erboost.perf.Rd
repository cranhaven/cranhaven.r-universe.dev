\name{erboost.perf}
\alias{erboost.perf}
\title{erboost performance}
\description{
Estimates the optimal number of boosting iterations for a \code{erboost} object and
optionally plots various performance measures
}
\usage{
erboost.perf(object, 
         plot.it = TRUE, 
         oobag.curve = FALSE, 
         overlay = TRUE, 
         method)
}
\arguments{
\item{object}{a \code{\link{erboost.object}} created from an initial call to 
\code{\link{erboost}}.}
\item{plot.it}{an indicator of whether or not to plot the performance measures.
Setting \code{plot.it=TRUE} creates two plots. The first plot plots 
\code{object$train.error} (in black) and \code{object$valid.error} (in red) 
versus the iteration number. The scale of the error measurement, shown on the 
left vertical axis, depends on the \code{distribution} argument used in the 
initial call to \code{\link{erboost}}.}
\item{oobag.curve}{indicates whether to plot the out-of-bag performance measures
in a second plot.}
\item{overlay}{if TRUE and oobag.curve=TRUE then a right y-axis is added to the 
training and test error plot and the estimated cumulative improvement in the loss 
function is plotted versus the iteration number.}
\item{method}{indicate the method used to estimate the optimal number
of boosting iterations. \code{method="OOB"} computes the out-of-bag
estimate and \code{method="test"} uses the test (or validation) dataset 
to compute an out-of-sample estimate. \code{method="cv"} extracts the 
optimal number of iterations using cross-validation if \code{erboost} was called
with \code{cv.folds}>1}
}
\value{
\code{erboost.perf} returns the estimated optimal number of iterations. The method 
of computation depends on the \code{method} argument.}
\references{
Yang, Y. and Zou, H. (2015), \dQuote{Nonparametric Multiple Expectile Regression via ER-Boost,} \emph{Journal of Statistical Computation and Simulation}, 84(1), 84-95.


G. Ridgeway (1999). \dQuote{The state of boosting,} \emph{Computing Science and
Statistics} 31:172-181.

\url{https://cran.r-project.org/package=gbm}\cr

}
\author{Yi Yang \email{yiyang@umn.edu} and Hui Zou \email{hzou@stat.umn.edu}}
\seealso{\code{\link{erboost}}, \code{\link{erboost.object}}}

\keyword{nonlinear}
\keyword{survival}
\keyword{nonparametric}
\keyword{tree}
