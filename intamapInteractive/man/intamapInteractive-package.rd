\name{intamapInteractive-package}
\alias{intamapInteractive-package}

\title{Interactive functionality added to the intamap package}

\description{
This package provides some added functionality to the 
\code{link[intamap]{intamap-package}} for automatic interpolation of environmental 
variables. Whereas \code{link[intamap]{intamap-package}} was specifically developed
as a statistical back-end for a Web Processing Service (WPS), this package 
offers some functionality that is not possible to access through such a WPS.

The methods in this package can mainly be put into three groups:

\describe{
  \item{bias correction}{methods for estimating and possible correct for 
          biases between measurement networks, due to differences in measurement
          strategies, measurement devices, or (unknown) post-processing of data}
  \item{segmentation}{method for segmentation of data, based on their measurement density}
  \item{network optimization}{methods for optimizing a measurement network
          (adding or removing observation points), based on different criteria}
}

}


\keyword{spatial}
