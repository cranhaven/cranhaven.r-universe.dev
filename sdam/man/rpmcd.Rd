\name{rpmcd}
\docType{data}
\alias{rpmcd}
\title{
Caption maps and affiliation dates of Roman provinces 
}
\description{
This is a list with specifications to plot caption maps of 59 Roman provinces (year 117 AD) 
and Italian regions under Emperor Augustus (year 27 BC). 
}
\usage{
data("rpmcd")
}
\format{
\code{rpmcd} is a list of lists for each province or region with two main components. 
One component is a list with shape data for a cartographical map caption in different slots, 
and the second component has for each Roman province an affiliation date when the territory became Roman province. 
\code{names(rpmcd)} have the acronyms according to \code{EDH} dataset.
}
\source{
https://commons.wikimedia.org/wiki/File:RomanEmpire_117.svg

https://commons.wikimedia.org/wiki/File:Roman_provinces_trajan.svg

https://commons.wikimedia.org/wiki/File:Regioni_dell'Italia_Augustea.svg
}
%\references{
%}
\seealso{
\code{retn}, \code{rpmp}, \code{\link{plot.map}}, \code{\link{rp}}, \code{EDH}
}
\keyword{datasets}
