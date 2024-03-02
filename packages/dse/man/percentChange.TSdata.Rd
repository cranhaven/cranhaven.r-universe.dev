\name{percentChange.TSdata}
\alias{percentChange.TSdata}
\alias{percentChange.TSestModel}
 
\title{Calculate percent change}
\description{Calculate the percent change relative to the data lag periods prior. 
}
\usage{
    \method{percentChange}{TSdata}(obj, base=NULL, lag=1, cumulate=FALSE, e=FALSE, ...)
    \method{percentChange}{TSestModel}(obj, base=NULL, lag=1, cumulate=FALSE, e=FALSE, ...)
    }
\arguments{
    \item{obj}{An object of class TSdata or TSestModel}
    \item{e}{see the default method.}
    \item{base}{see the default method.}
    \item{lag}{see the default method.}
    \item{cumulate}{see the default method.}
    \item{...}{arguments passed to other methods.}
}
\value{
For an object of class TSdata the percent change calculation is done
with the output data and the result is an object of class TSdata (or a list of
objects of class TSdata).
For an object of class TSestModel the percent change calculation is done
with \code{estimates$pred} and the result is an object of class TSdata (or a 
list of objects of class TSdata).
}
\details{See \code{\link[tfplot]{percentChange}}.}
\seealso{
\code{\link[tfplot]{percentChange}}
\code{\link[tfplot]{ytoypc}}
}
\examples{
data("eg1.DSE.data", package="dse")
z <- percentChange(outputData(eg1.DSE.data))
}
\keyword{ts}

