\name{plot.erboost}
\alias{plot.erboost}
\title{ Marginal plots of fitted erboost objects }
\description{
Plots the marginal effect of the selected variables by "integrating" out the other variables.
}
\usage{
\method{plot}{erboost}(x,
     i.var = 1,
     n.trees = x$n.trees,
     continuous.resolution = 100,
     return.grid = FALSE,
     ...)
}
\arguments{
  \item{x}{ a \code{\link{erboost.object}} fitted using a call to \code{\link{erboost}}}
  \item{i.var}{a vector of indices or the names of the variables to plot. If
  using indices, the variables are indexed in the same order that they appear
  in the initial \code{erboost} formula.
  If \code{length(i.var)} is between 1 and 3 then \code{plot.erboost} produces the plots. Otherwise,
  \code{plot.erboost} returns only the grid of evaluation points and their average predictions}
  \item{n.trees}{ the number of trees used to generate the plot. Only the first
  \code{n.trees} trees will be used}
  \item{continuous.resolution}{ The number of equally space points at which to
  evaluate continuous predictors }
  \item{return.grid}{ if \code{TRUE} then \code{plot.erboost} produces no graphics and only returns
  the grid of evaluation points and their average predictions. This is useful for
  customizing the graphics for special variable types or for dimensions greater
  than 3 }
  \item{\dots}{ other arguments passed to the plot function }
}
\details{
\code{plot.erboost} produces low dimensional projections of the
\code{\link{erboost.object}} by integrating out the variables not included in the
\code{i.var} argument. The function selects a grid of points and uses the
weighted tree traversal method described in Friedman (2001) to do the
integration. Based on the variable types included in the projection,
\code{plot.erboost} selects an appropriate display choosing amongst line plots,
contour plots, and \code{\link[lattice]{lattice}} plots. If the default graphics
are not sufficient the user may set \code{return.grid=TRUE}, store the result
of the function, and develop another graphic display more appropriate to the
particular example.
}
\value{
Nothing unless \code{return.grid} is true then \code{plot.erboost} produces no
graphics and only returns the grid of evaluation points and their average
predictions.
}
\references{
Yang, Y. and Zou, H. (2015), \dQuote{Nonparametric Multiple Expectile Regression via ER-Boost,} \emph{Journal of Statistical Computation and Simulation}, 84(1), 84-95.



G. Ridgeway (1999). \dQuote{The state of boosting,} \emph{Computing Science and
Statistics} 31:172-181.

\url{https://cran.r-project.org/package=gbm}\cr


J.H. Friedman (2001). "Greedy Function Approximation: A Gradient Boosting
Machine," Annals of Statistics 29(4).
}
\author{Yi Yang \email{yiyang@umn.edu} and Hui Zou \email{hzou@stat.umn.edu}}

\seealso{ \code{\link{erboost}}, \code{\link{erboost.object}}, \code{\link[graphics]{plot}} }

\keyword{ hplot }
