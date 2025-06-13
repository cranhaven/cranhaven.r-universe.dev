\name{fit.control}
\alias{fit.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Auxiliary function for controlling model fitting
}
\description{
Auxiliary function as user interface for model fitting. Typically
     only used when calling 'jumpoints'
     }
\usage{
fit.control(toll = 0.001, it.max = 5, display = FALSE, last = TRUE, 
    maxit.glm = 25, h = 1, stop.if.error = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{toll}{
  positive convergence tolerance.
}
  \item{it.max}{
integer giving the maximal number of iterations.
}
  \item{display}{
logical indicating if the value of the objective function should be printed at each iteration.
}
  \item{last}{
Currently ignored.
}
  \item{maxit.glm}{
Currently ignored.
}
  \item{h}{
Currently ignored.
}
  \item{stop.if.error}{
logical indicating if the algorithm should stop when one or more estimated changepoints 
do not assume admissible values. Default is \code{FALSE} which implies automatic 
changepoint selection.

}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
A list with the arguments as components to be used by 'jumpoints'.

}
\author{ Vito M. R. Muggeo }

\seealso{\code{\link{jumpoints}}}

\keyword{ regression }
