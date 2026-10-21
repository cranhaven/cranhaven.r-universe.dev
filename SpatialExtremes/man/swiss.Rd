\name{swiss}
\docType{data}
\alias{swiss}
\title{
  Map of the Switzerland.
}
\description{
  Plot a map of Switzerland and optionnaly some cities.
}
\usage{
swiss(city = FALSE, add = FALSE, axes = FALSE, km = TRUE, xlab = "",
ylab = "", ...)
}
\arguments{
  \item{city}{Logical. It \code{TRUE}, some city are displayed. Default
    is to omit.}
  \item{add}{Logical. Should the map be added to an existing plot?}
  \item{axes}{Logical. Should the axis be displayed?}
  \item{km}{Logical. If \code{TRUE} (default) the longitude and latitude
    are expressed in kilometers. Otherwise it is in meters.}
  \item{xlab,ylab}{The x and y-axis labels.}
  \item{\dots}{Optional arguments to be passed to the \code{\link{plot}}
    function.}
}
\value{
  A graphic window.
}
\author{Dominik Schaub}
\examples{
swiss()
}
\keyword{hplot}
