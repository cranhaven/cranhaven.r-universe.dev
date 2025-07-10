\name{Mnist}
\alias{Mnist}
\docType{data}
\title{
 Subset of MNIST training and testing data.
}
\description{
10000 MNIST training images (1000 of every digit), reformatted into a tensor: 28 x 10000 x 28.
1000 MNIST test images (100 of every digit), reformatted into a tensor: 28 x 1000 x 28
}
\usage{data("Mnist")}
\format{
  The format is:

    Mnist$train$images, Mnist$train$labels

    Mnist$test$images, Mnist$test$labels
}

\references{
Deng L (2012). “The mnist database of handwritten digit images for machine learning research.” IEEE Signal Processing Magazine, 29(6), 141–142

}
\examples{
data("Mnist")
}
\keyword{datasets}
