\name{forget}
\alias{forget}
\title{
Forgets the Continuation
}
\description{
This function takes a function of the form \code{f(x, ret)} and forgets the \code{ret} part returning a function of the form \code{g(x)}.
}
\usage{
forget(f)
}
\arguments{
  \item{f}{
a function of the form \code{f(x, ret)}.
}
}
\value{
a function of the form \code{f(x)}.
}
\author{
Matteo Provenzano\cr
\url{http://www.alephdue.com}
}
\seealso{
\code{\link{compose}}
}
\examples{
# forget the FUN part in lapply
to.list <- forget(lapply)

# returns the list of the natural numbers from 1 to 10
to.list(1:10)
}
