\name{Wulffpoint}
\alias{Wulffpoint}
\title{Function Wulffpoint
Plots Points in the Wulffnet given S and ES}
\usage{
Wulffpoint(ES, S, col = 2, pch = 5, bg="white" , lab = "")
}
\arguments{
\item{ES}{(numeric) azimuth (extinction angle) in degrees}

\item{S}{(numeric) spindle angle in degrees}

\item{col}{color}

\item{pch}{symbol type}

\item{lab}{label}

\item{bg}{background colour of symbol}
}
\description{
Function Wulffpoint
Plots Points in the Wulffnet given S and ES
}
\author{
Florian Dufey <GrimR@gmx.de>
}
\seealso{Wnet}
\examples{
Wulffnet()
Wulffpoint(23, 34)
}

