\name{textile-package}
\alias{textile-package}
\alias{textile}
\alias{trainImg}
\alias{icImgs}
\alias{ocImgs}
\docType{data}
\title{
Textile Images
}
\description{
Real images of the same textile material with/without local defects, which were used in Bui and Apley (2018).
}
\usage{
data("trainImg")
data("icImgs")
data("ocImgs")
}
\format{
  The data set contains the following:
  \describe{
    \item{\code{trainImg}}{an integer matrix}
    \item{\code{icImgs}}{a 4-dimensional integer array}
    \item{\code{ocImgs}}{a 4-dimensional integer array}
  }
}
\details{
The images were taken at a scale that shows their weave patterns, which exhibit a great deal of stochastic behavior. They are examples of the stochastic textured surface data, defined in Bui and Apley (2018). These images were used in Bui and Apley (2018) to demonstrate their general approach for monitoring and diagnosing stochastic textured surface data for local defects. The data set contains three objects:

\code{trainImg } stores the greyscale values of a textile image of size 500x500 pixels without local defects. It can be used to train a supervised learning model that captures the normal in-control statistical behavior of the textile.

\code{icImgs } stores the greyscale values of 94 textile images of size 250x250 pixels without local defects. These images can be used to establish the control limits and diagnostic thresholds.

\code{ocImgs } stores the greyscale values of 6 textile images of size 250x250 pixels with local defects. These images can be used to test the constructed control limits and diagnostic thresholds.
}
\references{
Bui, A.T., and Apley., D.W. (2018). A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces. Technometrics, 60, 1-13.
}
\examples{
data("trainImg")
image(1:ncol(trainImg), 1:nrow(trainImg), as.matrix(t(apply(trainImg , 2, rev))),
          col = gray((0:32)/32), xlab="", ylab="")

data("icImgs")
image(1:dim(icImgs)[1], 1:dim(icImgs)[2], as.matrix(t(apply(icImgs[,,1] , 2, rev))),
          col = gray((0:32)/32), xlab="", ylab="")

data("ocImgs")
image(1:dim(ocImgs)[1], 1:dim(ocImgs)[2], as.matrix(t(apply(ocImgs[,,1] , 2, rev))),
          col = gray((0:32)/32), xlab="", ylab="")
}
\keyword{datasets}
