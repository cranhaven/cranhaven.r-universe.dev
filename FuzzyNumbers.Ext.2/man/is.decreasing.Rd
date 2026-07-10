\name{is.decreasing}
\alias{is.decreasing}
\title{
Diagnosis a decreasing function
}
\description{
\code{is.decreasing} tests if the introduced one-variable function is decreasing (or in fact, non-increasing) on the considered \code{x.bound} or not. 
In other words, \code{is.decreasing} returns \code{TRUE} if the introduced function is decreasing on the considered \code{x.bound}; and it returns \code{FALSE} otherwise.
The goal of introducing function \code{is.decreasing} in package \code{FuzzyNumbers.Ext.2} is using in function \code{f2apply}.}
\usage{
is.decreasing(fun, x.bound = c(-1, 1), step = 0.01)
}
\arguments{
  \item{fun}{
a one-variable R function
}
  \item{x.bound}{
a vector with two real ordered elements which determine a bound on x-axis for checking the monotonic of the considered function
}
  \item{step}{
a positive real-valued number which determine the increment of the considered sequence for checking the monotonic of the considered function. The default of \code{step} is 0.01. 
Increasing \code{step} value can cause the decreasing the time of computation and also couse the decreasing the precision of the calculations.
}
}
\value{
\code{TRUE} for decreasing one-variable functions on the considered \code{x.bound}; otherwise \code{FALSE}
}

\seealso{
\code{is.increasing}
}
\examples{
is.decreasing(fun=function(x) -2*x+10, x.bound=c(4,6), step=.1)

g = function(x) x^3
is.decreasing(g, x.bound=c(-24,6))

## The function is currently defined as
function (fun, x.bound = c(-1, 1), step = 0.01) 
{
    x = seq(x.bound[1], x.bound[2], by = step)
    i = 1
    while (fun(x[i]) >= fun(x[i + 1])) {
        if (i < length(x) - 1) {
            i <- i + 1
        }
        else (return(TRUE))
    }
    return(FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
\keyword{ monoton function }
\keyword{ is.increasing }
\keyword{ is.decreasing }
\keyword{ is.increasing.on.x }
\keyword{ is.decreasing.on.x }
\keyword{ is.increasing.on.y }
\keyword{ is.decreasing.on.y }


