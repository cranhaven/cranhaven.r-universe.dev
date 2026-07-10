\name{is.increasing.on.x}
\alias{is.increasing.on.x}
\title{
Diagnosis an increasing two-variable function toward x
}
\description{
\code{is.increasing.on.x} tests for any fixed \eqn{y} from \code{y.bound}, if the introduced two-variable function \eqn{f(x,y)} is increasing toward \eqn{x} on the considered \code{x.bound} or not. 
In other words, \code{is.increasing.on.x} returns \code{TRUE} if the introduced function \eqn{f(x,y)} is increasing function of \eqn{x} on the considered \code{x.bound} (for any fixed \eqn{y} in \code{y.bound}); and it returns \code{FALSE} otherwise.
The goal of introducing function \code{is.increasing.on.x} in package \code{FuzzyNumbers.Ext.2} is using in function \code{f2apply}.
}
\usage{
is.increasing.on.x(fun, x.bound = c(-1, 1), y.bound = c(-1, 1), step = 0.01)
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
\code{TRUE} for two-variable function f(x,y) which is increasing toward x on \code{x.bound} (for any fixed \eqn{y} from \code{y.bound}); and otherwise \code{FALSE}
}

\seealso{
\code{is.increasing}, \code{is.increasing.on.y}
}
\examples{
is.increasing.on.x(fun=function(x,y) 2*x+y, x.bound=c(0,2), y.bound=c(1,2), step=.2)

f = function(x,y) x^2+y
is.increasing.on.x(f, x.bound=c(0,2), y.bound=c(0,2))
is.increasing.on.x(f, x.bound=c(-2,2), y.bound=c(0,2))
is.increasing.on.x(f, x.bound=c(0,2), y.bound=c(-2,2))

## The function is currently defined as
function (fun, x.bound = c(-1, 1), y.bound = c(-1, 1), step = 0.01) 
{
    y = seq(y.bound[1], y.bound[2], by = step)
    for (i in 1:length(y)) {
        g = function(x) fun(x, y[i])
        if (is.increasing(g, x.bound, step) == FALSE) {
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

