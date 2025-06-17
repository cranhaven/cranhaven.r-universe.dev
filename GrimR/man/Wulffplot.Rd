\name{Wulffplot}
\alias{Wulffplot}
\title{Function Wulffplot}
\usage{
Wulffplot(x)
}
\arguments{
\item{x}{(list) Output list from the fit.joel function}
}
\description{
Function Wulffplot
Plot the S and ES values of measured points calculated points and of all axes on a Wulff stereonet
}
\author{
Florian Dufey <GrimR@gmx.de>
}
\examples{
res<-fit.joel(Gunter,MR=-0.89,cw="cw",optimMR=FALSE)
Wulffplot(res)
}

