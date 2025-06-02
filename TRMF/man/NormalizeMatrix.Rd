\name{NormalizeMatrix}
\alias{NormalizeMatrix}
\title{
Matrix Scaling }
\description{
A function for normalizing (scaling and centering) a matrix.
}
\usage{
NormalizeMatrix(X, method = c("standard", "robust", "range", "none"),
            type = c("global", "rowwise", "columnwise"), na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{a numeric matrix(like object)}
  \item{method}{type of scaling to perform, \code{standard} centers with mean, and scales by \code{sd()}, \code{robust} centers with the median and scales by \code{mad(,constant=1)},  \code{range} maps to \code{[0-1]} interval}
  \item{type}{how should normalization be applied. \code{global} scales and centers matrix by one value. \code{columnwise} and \code{rowwise}
  normalize each column or row separately.}
  \item{na.rm}{logical value, ignore NA values or not.
}
}
\details{
Scaling and centering quantities are stored as attributes.
}
\value{
The possibly centered and scaled matrix. Scaling and centering quantities are stored as attributes.
}

\author{
Chad Hammerquist
}


\examples{
x = matrix(1:10, ncol = 2)
NormalizeMatrix(x)
}
