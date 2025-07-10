\name{SShorton}
\alias{SShorton}
\title{
A Horton water infiltration model
}
\description{
This is a three- parameter function to determine water infiltration parameters contained in the Horrton infiltration model
}
\usage{
SShorton(input, fc, f0, lrk)
}
\arguments{
  \item{input}{
Time
}
  \item{fc}{
Steady infiltration rate
}
  \item{f0}{
Initial infiltration rate
}
  \item{lrk}{
Shape factor related to soil pore continuity
}
}
\details{
This is a self-starting model for estimating parameters in the Horton infiltration model using instantaneous infiltration rates (here known as Rate) and time data (Time)
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Horton RE. 1940. An approach towards a physical interpretation of infiltration capacity. Soil Science Society of America Proceedings 5: 227-237
}
\author{
Christian Thine Omuto
}

\seealso{
\code{\link{SSphilip}}, \code{\link{SSgampt}}
}
\examples{
data(infilt)
infil <- infilt[319:751,]
library(nlme)
hort.lis <- nlsList(log(Rate) ~ SShorton(Time,fc,f0,lrk) | PlotNo,
                    infil) # Rate is the y variable
hort.lis
}
\keyword{ models }
\keyword{ manip }
