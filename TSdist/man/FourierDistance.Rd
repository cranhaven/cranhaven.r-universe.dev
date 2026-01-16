\name{FourierDistance}
\alias{FourierDistance}

\title{
Fourier Coefficient based distance. 
}
\description{
Computes the distance between a pair of numerical series based on their Discrete Fourier Transforms.
}

\usage{
FourierDistance(x, y, n = (floor(length(x) / 2) + 1))
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{n}{
  Positive integer that represents the number of Fourier Coefficients to consider.
  ( default=(floor(length(x) / 2) + 1) )
}
}

\details{
The Euclidean distance between the first \code{n} Fourier coefficients of series \code{x} and \code{y} is computed. The series must have the same length. Furthermore, \code{n} should not be larger than the length of the series.

%\bold{Note:} This distance measure is based on the Euclidean distance. This means that it can be used directly within kernel machines, for example by inserting it in the Gaussian RBF kernel, because it will provide positive definite Gram matrices. . 

}

\value{
\item{d}{
   The computed distance between the pair of series.
}


}

\references{
Agrawal, R., Faloutsos, C., & Swami, A. (1993). \emph{Efficient similarity search in sequence databases}. In Proceedings of the 4th International Conference of Foundations of Data Organization and Algorithms (Vol. 5, pp. 69-84).
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}



\seealso{
To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.
}
\examples{

# The objects example.series1 and example.series2 are two 
# numeric series of length 100 contained in the TSdist package. 

data(example.series1)
data(example.series2)

# For information on their generation and shape see help 
# page of example.series.

help(example.series)

# Calculate the Fourier coefficient based  distance using 
# the default number of coefficients:

FourierDistance(example.series1, example.series2)

# Calculate the Fourier coefficient based  distance using 
# only the first 20 Fourier coefficients:

FourierDistance(example.series1, example.series2, n=20)

}
