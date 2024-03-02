\name{l}
\alias{l}
\alias{l.TSdata}
\alias{l.TSestModel}

\title{Evaluate a TSmodel}
\description{Evaluate a model with data.}
\usage{
    l(obj1, obj2, ...)
    \method{l}{TSdata}(obj1, obj2, ...)
    \method{l}{TSestModel}(obj1, obj2, ...)
}
\arguments{
    \item{obj1}{a TSmodel, TSdata, or TSestModel object.}
    \item{obj2}{a TSmodel or TSdata object.}
    \item{...}{arguments to be passed to other methods.}
}
\value{Usually a TSestModel object is returned. Most methods allow an
  argument \code{result} which specifies that a certain part of the object is
  returned. (This is passed in \dots to another method in most cases.)
  The likelihood can be returned by specifying result="like", which is useful
  for optimization routines.}
\details{
For state space models \code{l.SS} is called and for 
ARMA models \code{l.ARMA} is called.
}
\seealso{
\code{\link{l.SS}},
\code{\link{l.ARMA}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- toSS(TSmodel(estVARXls(eg1.DSE.data.diff)))
evaluated.model <- l(model, eg1.DSE.data.diff)
}
\concept{DSE}
\keyword{ts}


