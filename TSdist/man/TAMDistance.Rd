\name{TAMDistance}
\alias{TAMDistance}

\title{
Time Alignment Measurement (TAM) distance. 
}
\description{
Computes the Time Alignment Measurement between a pair of 
numeric time series.
}

\usage{
TAMDistance(x, y)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}
  \item{y}{
  Numeric vector containing the second time series.
}

}
\details{
The Time Alignment Measurement (TAM) between two numeric series is calculated. Quantifies the degree of temporal distortion between two time series.

The main idea behind TAM is to measure the warping cost between a given time series and another. TAM is calculated from the optimal alignment warping path between two time series provided by \code{\link[dtw]{dtw}}, which allows characterizing the intervals when the series are in phase, advance or delay. This distance penalizes signals where advance or delay is present and benefits series that are in phase with each other. As the distance increases, the dissimilarity between both signals also increases. The distance is bounded between 0 (both series are in phase) and 3 (both series are completely out-of-phase).

The length of series \code{x} and \code{y} may be different.  


%\bold{Note:} This distance measure is not a metric (Chen and Ng, 2004), and, is not negative definite (Lei and Sun, 2007). This means that it can not be used directly within kernel machines  in all cases (for example, by inserting it in the Gaussian RBF kernel), because it may provide a non positive semi-definite Gram matrix, which is a requirement for kernel based classifiers such as Support Vector Machines or Gaussian Processes. As such, the positive definiteness of the obtained Gram matrix should be analyzed in advance. More information and some solutions to this problem can be found in (Cuturi, 2011; Gaidon et al. 2011; Marteau and Gibet, 2014; Lei and Sun, 2007; Pree, 2014)


}

\value{
\item{d}{
  The computed distance between the pair of series.
}
}

\references{
Duarte Folgado, Marília Barandas, Ricardo Matias, Rodrigo Martins, Miguel Carvalho, Hugo Gamboa (2018). Time Alignment Measurement for Time Series. Pattern Recognition 81, pp. 268-279.
}

\author{
Duarte Folgado
}

\seealso{
To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.
}

\examples{

# The objects example.series1 and example.series2 are two 
# numeric series of length 100 contained in the TSdist package. 

data(example.series1)
data(example.series2)

# The objects example.series3 and example.series4 are two 
# numeric series of length 100 and 120 contained in the TSdist
# package. 

data(example.series3)
data(example.series4)

# For information on their generation and shape see 
# help page of example.series.

help(example.series)

# Calculate the TAM distance for two series of the same length:

TAMDistance(example.series1, example.series2)

# Calculate the TAM distance for two series of different length:

TAMDistance(example.series3, example.series4)


}
