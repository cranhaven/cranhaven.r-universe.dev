\name{demopat}
\encoding{latin1}
\alias{demopat}
\docType{data}
\title{Artificial Data Point Pattern from \code{spatstat.data} package.}
\description{
This is an artificial dataset, for use in testing and demonstrating compatibility between \code{spatstat} and \code{ads} objects. It is a multitype point pattern in an irregular polygonal window.
 There are two types of points. The window contains a polygonal hole.
}
\usage{data(demopat)}
\format{
An object of class "ppp" representing a \code{spatstat.core} point pattern.
}
\source{
 data(demopat) in \code{spatstat.data} 
}
\examples{
	data(demopat)
	demo.spp<-ppp2spp(demopat)
	plot(demo.spp)
}
\keyword{datasets}
