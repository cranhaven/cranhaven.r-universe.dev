\name{is.decreasing.on.y}
\alias{is.decreasing.on.y}
\title{
Diagnosis a decreasing two-variable function toward y
}
\description{
\code{is.decreasing.on.y} tests for any fixed \eqn{x} from \code{x.bound}, if the introduced two-variable function \eqn{f(x,y)} is decreasing toward \eqn{y} on the considered \code{y.bound} or not. 
In other words, \code{is.decreasing.on.y} returns \code{TRUE} if the introduced function \eqn{f(x,y)} is decreasing function of \eqn{y} on the considered \code{y.bound} (for any fixed \eqn{x} in \code{x.bound}); and it returns \code{FALSE} otherwise.
The goal of introducing function \code{is.decreasing.on.y} in package \code{FuzzyNumbers.Ext.2} is using in function \code{f2apply}.
}
\usage{
is.decreasing.on.y(fun, x.bound = c(-1, 1), y.bound = c(-1, 1), step = 0.01)
}
\arguments{
  \item{fun}{
a two-variable R function
}
  \item{x.bound}{
a vector with two real ordered elements which determine a bound on x-axis for checking the monotonic
}
  \item{y.bound}{
a vector with two real ordered elements which determine a bound on y-axis for checking the monotonic
}
  \item{step}{
a positive real-valued number which determine the increment of the considered sequence for checking the monotonic of the considered function. The default of \code{step} is 0.01. 
Increasing \code{step} value can cause the decreasing the time of computation and also cause the decreasing the precision of the calculations.
}
}
\value{
\code{TRUE} for two-variable function f(x,y) which is decreasing toward y on \code{y.bound} (for any fixed \eqn{x} from \code{x.bound}); and otherwise \code{FALSE}
}

\seealso{
\code{is.decreasing}, \code{is.decreasing.on.x}
}
\examples{
is.decreasing.on.y(fun=function(x,y) 2*x-y, x.bound=c(0,2), y.bound=c(1,2), step=.2)

H = function(x,y) pnorm(x)-pnorm(y)
is.decreasing.on.x(H)
is.decreasing.on.y(H)

## The function is currently defined as
function (fun, x.bound = c(-1, 1), y.bound = c(-1, 1), step = 0.01) 
{
    x = seq(x.bound[1], x.bound[2], by = step)
    for (i in 1:length(x)) {
        g = function(y) fun(x[i], y)
        if (is.decreasing(g, y.bound, step) == FALSE) {
            return(FALSE)
        }
    }
    return(TRUE)
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
