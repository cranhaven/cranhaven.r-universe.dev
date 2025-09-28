\name{getCloud2D}
\alias{getCloud2D}
\title{retrieves a given 2D cloud by it's name from the given file and returns
it as a data.frame}
\usage{
getCloud2D(fileName, cloudName)
}
\arguments{
  \item{fileName}{name of the AIDA file}

  \item{cloudName}{name of the AIDA 2D cloud to be
  returned}
}
\description{
retrieves a given 2D cloud by it's name from the given file
and returns it as a data.frame
}
\examples{
histoFile = system.file("extdata", "clouds.xml.gz", package="aidar")
c2d = getCloud2D(histoFile, '30')
}
\keyword{aida}
\keyword{cloud}

