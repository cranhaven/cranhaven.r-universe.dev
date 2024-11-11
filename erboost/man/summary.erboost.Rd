\name{summary.erboost}
\alias{summary.erboost}
\title{Summary of a erboost object }
\description{
Computes the relative influence of each variable in the erboost object.
}
\usage{
\method{summary}{erboost}(object,
        cBars=length(object$var.names),
        n.trees=object$n.trees,
        plotit=TRUE,
        order=TRUE,
        method=relative.influence,
        normalize=TRUE,
        ...)
}
\arguments{
\item{object}{a \code{erboost} object created from an initial call to
\code{\link{erboost}}.}
\item{cBars}{ the number of bars to plot. If \code{order=TRUE} the only the
variables with the \code{cBars} largest relative influence will appear in the
barplot. If \code{order=FALSE} then the first \code{cBars} variables will
appear in the plot. In either case, the function will return the relative
influence of all of the variables.}
\item{n.trees}{ the number of trees used to generate the plot. Only the first
\code{n.trees} trees will be used.}
\item{plotit}{ an indicator as to whether the plot is generated. }
\item{order}{ an indicator as to whether the plotted and/or returned relative
influences are sorted. }
\item{method}{ The function used to compute the relative influence.
\code{\link{relative.influence}} is the default and is the same as that
described in Friedman (2001). The other current (and experimental) choice is
\code{\link{permutation.test.erboost}}. This method randomly permutes each predictor
variable at a time and computes the associated reduction in predictive
performance. This is similar to the variable importance measures Breiman uses
for random forests, but \code{erboost} currently computes using the entire training
dataset (not the out-of-bag observations.}
\item{normalize}{ if \code{FALSE} then \code{summary.erboost} returns the 
unnormalized influence. }
\item{...}{ other arguments passed to the plot function. }
}
\details{
This returns the reduction attributeable to each varaible in sum of squared error in 
predicting the gradient on each iteration. It describes the relative influence 
of each variable in reducing the loss function. See the references below for 
exact details on the computation.
}
\value{
Returns a data frame where the first component is the variable name and the
second is the computed relative influence, normalized to sum to 100.
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

\seealso{ \code{\link{erboost}} }

\keyword{ hplot }
