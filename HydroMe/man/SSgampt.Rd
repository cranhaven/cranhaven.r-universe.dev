\name{SSgampt}
\alias{SSgampt}
\title{
A Green-Ampt water infiltration model
}
\description{
This is a two- parameter function to determine water infiltration parameters contained in the Green-Ampt infiltration model
}
\usage{
SSgampt(input, ks, A)
}
\arguments{
  \item{input}{
Instantaneous infiltration rate
}
  \item{ks}{
Saturated hydraulic conductivity
}
  \item{A}{
Constant incorporating matric suction potential, wetting front, and antecedent moisture content
}
}
\details{
This is a self-starting model for estimating Green-Ampt infiltration model using instantaneous and cumulative infiltration data
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Green WA and Ampt GA. 1911. Studies on soil physics: 1. The flow of air and water through soils. Journal of Agricultural Science 4: 1-24
}
\author{
Christian Thine Omuto
}

\seealso{
\code{\link{SSphilip}}, \code{\link{SShorton}}
}
\examples{
## Load the data
data(infilt)
library(minpack.lm)
gamp.ns <- nlsLM(Cumrate ~ SSgampt(Rate,ks,A), infilt)
summary(gamp.ns)
}
\keyword{ models }
\keyword{ manip }
