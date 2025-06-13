\name{bilinearInterpolator}
\alias{bilinearInterpolator}

\title{
A function to perform bilinar interpolation 
}
\description{
This function is just a wrapper to the \code{\link{interp.surface.grid}} function to perform bilinar interpolation of a regular matrix.
}
\usage{
bilinearInterpolator(oldMatrix, pointsInNewX, pointsInNewY)
}
\arguments{
  \item{oldMatrix}{
  The original matrix.
}
  \item{pointsInNewX}{
  The number of points in the new matrix (the number of new rows).
}
  \item{pointsInNewY}{
  The number of points in the new matrix (the number of new columns).
}
}
\value{
It returns an interpolated matrix. The size of the new matrix is pointsInNewX rows and pointsInNewY columns. 
}
\author{
Alberto Krone-Martins
}

\seealso{
\code{\link{interp.surface.grid}}
}
\examples{
library('jpeg')
 
# Read the R logo
logo <- readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))

# Create a scaled down version of the R channel
intrpArray <- array(dim=c(20, 20, 3))
intrpArray[,,1] <- bilinearInterpolator(logo[,,1], dim(intrpArray)[1], dim(intrpArray)[2])

# Display the results
dev.new()
image(logo[,,1], main="Original")
dev.new()
image(intrpArray[,,1], main="Scaled down")
}

\keyword{ utilities }
\keyword{ misc }
