\name{samplePts}
\alias{samplePts}

\title{
sample \emph{n} point locations in (or on) a spatial object 
}
\description{
sample location points within a square area, a grid, a polygon, or on a spatial line, using regular or random sampling methods. The function \code{spsample} from the package \code{sp} is used iteratively to find exactly \emph{n} sample locations
}
\usage{
samplePts(x, n, type, ...)
}

\arguments{
\item{x}{Spatial object; see the \code{sp} package for details}
\item{n}{exact sample size}
\item{type}{character; "random" for completely spatial random; "regular" for regular (systematically aligned) sampling; "stratified" for stratified random (one single random location in each "cell"); "nonaligned" for nonaligned systematic sampling (nx random y coordinates, ny random x coordinates); "hexagonal" for sampling on a hexagonal lattice; "clustered" for clustered sampling; "Fibonacci" for Fibonacci sampling on the sphere. See the \code{sp} package for details}
\item{...}{other arguments to be passed to \code{spsample}}
}
\value{
an object of class \code{\link[sp]{SpatialPoints-class}}
}
\seealso{
See \code{\link[sp]{spsample}} in the \code{sp} package 
}
\examples{
data(lalib)
hexPts <- samplePts(lalib, 5, "hexagonal")
plot(lalib, xlim=c(bbox(lalib)[1], bbox(lalib)[3]), ylim=c(bbox(lalib)[2],
    bbox(lalib)[4]))
plot(hexPts, add=TRUE)
\dontrun{
randomPts <- samplePts(lalib, 5, "random")
plot(lalib, xlim=c(bbox(lalib)[1], bbox(lalib)[3]), ylim=c(bbox(lalib)[2],
    bbox(lalib)[4]))
plot(randomPts, add=TRUE)
}
}

\keyword{ spatial }
