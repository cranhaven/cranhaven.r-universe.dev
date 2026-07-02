\name{rpcp}
\docType{data}
\alias{rpcp}
\title{
Roman provinces and chronological periods
}
\description{
This dataset is a list of two data frames with 45 Roman provinces and regions with 
chronological periods of early and late Roman influence dates as time intervals. 
}
\usage{
data("rpcp")
}
\format{
A list with two data frames named \code{Early} and \code{Late} of size 45 \eqn{\times} 3 
with ancient Roman provinces as \code{Province} where each data frame represent an historical period. 
The row names in each data frame records the acronyms of the Roman province.

Time intervals in the first data frame that corresponds to the \code{Early} period of influence in provinces 
and regions are \code{EarInf} and \code{OffPrv}, while time intervals in the second data frame for the \code{Late} 
period of influence are \code{LateInf} and \code{Fall} with the year of fall from the Roman Empire.
}
\source{
https://www.unrv.com/provinces/provincetable.php (Retrieved on 2 July 2021)
}
%\references{
%}
\seealso{
\code{\link{rp}}, \code{\link{plot.dates}}
}
\keyword{datasets}
