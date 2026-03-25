\name{Menteith}
\alias{Menteith}
\docType{data}
\title{
Grey-level image of the Lake of Menteith%%   ~~ data name/kind ... ~~
}
\description{
This dataset is a 100x100 pixel satellite image of the lake of Menteith,
near Stirling, Scotland. The purpose of analyzing this satellite dataset is to
classify all pixels into one of six states in order to detect some
homogeneous regions.
}
\usage{data(Menteith)}
\format{
  data frame of a 100 x 100 image with 106 grey levels
}
\seealso{
\code{\link{reconstruct}}
}
\examples{
data(Menteith)
image(1:100,1:100,as.matrix(Menteith),col=gray(256:1/256),xlab="",ylab="")
}
\keyword{datasets}
\keyword{Menteith}
