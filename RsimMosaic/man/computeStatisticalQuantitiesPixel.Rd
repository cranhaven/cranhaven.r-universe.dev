\name{computeStatisticalQuantitiesPixel}
\alias{computeStatisticalQuantitiesPixel}

\title{
A function to compute the pixel data in a certain parameter space
}
\description{
A function to compute the relevant pixel quantity (the RGB color). Optionally it can also output the values of relevant, nearby pixels as RGB colors at the Left, UpperLeft, Upper, UpperRight, Right, LowerRight, Lower, and LowerLeft pixels.
}
\usage{
computeStatisticalQuantitiesPixel(i, j, img, useGradients = FALSE)
}

\arguments{
  \item{i}{
  The row (or the X in the image).
}
  \item{j}{
  The column (or the Y in the image).
}
  \item{img}{
  An image array (as created by the readJPEG function from the jpeg library).
}
  \item{useGradients}{
  A flag indicating if the values of the nearby pixels should be returned.
}
}
\value{
 An array with the relevant pixel quantites.
}
\author{
Alberto Krone-Martins
}

\seealso{
\code{\link{computeStatisticalQuantitiesTile}}
}
\examples{
# Read the R logo and output the value of its pixel (50, 5) in the parameter space
library('jpeg')
logo <- readJPEG(system.file("img", "Rlogo.jpg", package="jpeg")) # Read the R logo
computeStatisticalQuantitiesPixel(50, 5, logo) # Compute the quantities at the pixel (5,5)

}

\keyword{ utilities }
\keyword{ misc }
