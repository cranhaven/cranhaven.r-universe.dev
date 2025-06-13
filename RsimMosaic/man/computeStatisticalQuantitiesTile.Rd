\name{computeStatisticalQuantitiesTile}
\alias{computeStatisticalQuantitiesTile}

\title{
A function to compute the tile data in a certain parameter space
}
\description{
A function to compute the relevant statistical quantities (only the median value is implemented) of the RGB colors for the entire image. Optionally it can also compute the median values of the RGB colors at the Left, UpperLeft, Upper, UpperRight, Right, LowerRight, Lower, and LowerLeft, corners of the image.
}
\usage{
computeStatisticalQuantitiesTile(img, useGradients = FALSE)
}
\arguments{
  \item{img}{
  An image array (as created by the readJPEG function from the jpeg library).
}
  \item{useGradients}{
  A flag indicating if the values of the relevant quantities should be calculated in the corners of the image.
}
}
\details{
 The data is defined as the median RGB colors of the tile images at this version. Optionally, the median RGB values of the tile image corners are also calculated.
}
\value{
An array with the relevant quantities calculated.
}

\author{
Alberto Krone-Martins
}

\seealso{
\code{\link{readJPEG}}, \code{\link{computeStatisticalQuantitiesPixel}}
}

\examples{
# Read the R logo and output its values in the parameter space
library('jpeg')
logo <- readJPEG(system.file("img", "Rlogo.jpg", package="jpeg")) # Read the R logo
computeStatisticalQuantitiesTile(logo) # Compute the quantities

}

\keyword{ utilities }
\keyword{ misc }
