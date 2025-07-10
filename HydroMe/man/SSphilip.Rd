\name{SSphilip}
\alias{SSphilip}
\title{
A Philip's water infiltration model
}
\description{
This is a two- parameter function to determine water infiltration parameters contained in the Philip's infiltration model
}
\usage{
SSphilip(input, fc, S)
}
\arguments{
  \item{input}{
Time
}
  \item{fc}{
Steady infiltration rate
}
  \item{S}{
Sorptivity
}
}
\details{
This is a self-starting model for estimating Philip's infiltration model using instantaneous infiltration rates (here known as Rate) and time (Time) data
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Philip JR. 1957. The theory of infiltration.: 4 Sorptivity and algebraic infiltration equations. Soil Science 84: 257-264
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{SShorton}}, \code{\link{SSgampt}}
}
\examples{
data(infilt)
library(nlme)
philip.nis <- nlsList(log(Rate) ~ SSphilip(Time,fc,S) | PlotNo, data=infilt)
coef(philip.nis)
}

\keyword{ models }
\keyword{manip}
