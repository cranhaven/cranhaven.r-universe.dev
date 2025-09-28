\name{getCloud3D}
\alias{getCloud3D}
\title{retrieves a given 3D cloud by it's name from the given file and returns
it as a data.frame}
\usage{
getCloud3D(fileName, cloudName)
}
\arguments{
  \item{fileName}{name of the AIDA file}

  \item{cloudName}{name of the AIDA 3D cloud to be
  returned}
}
\description{
retrieves a given 3D cloud by it's name from the given file
and returns it as a data.frame
}
\examples{
histoFile = system.file("extdata", "clouds.xml.gz", package="aidar")
c3d = getCloud3D(histoFile, '33')
}
\keyword{aida}
\keyword{cloud}

