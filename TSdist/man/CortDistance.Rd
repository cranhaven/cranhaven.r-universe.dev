\name{CortDistance}
\alias{CortDistance}

\title{
Dissimilarity Index Combining Temporal Correlation and Raw Value
Behaviors
}

\description{
Computes the dissimilarity between two numeric series of the same length by combining the 
dissimilarity between the raw values and the dissimilarity between the temporal correlation behavior of the series.
}

\usage{
CortDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{...}{
   Additional parameters for the function. See  \code{\link[TSclust]{diss.CORT}} for more 
  information.
}
}

\details{
This is simply a wrapper for the \code{\link[TSclust]{diss.CORT}} function of package \pkg{TSclust}. As such, all the functionalities of the \code{\link[TSclust]{diss.CORT}} function are also available when using this function. 

%\bold{Note:} This distance measure is not a metric (Chouakria and Nagabhushan, 2007), but the negative definiteness is not explicitly mentioned in the literature, to the extent of our knowledge. As such, before using it directly within kernel machines (e.g. by inserting it in the Gaussian RBF kernel), we recommend analyzing the positive definiteness of the Gram matrix, which is a requirement for kernel based classifiers such as Support Vector Machines or Gaussian Processes. More information and some solutions to this problem can be found in (Pree, 2014).
}

\value{
\item{d}{
  The computed distance between the pair of series.
}
}

\references{
Pablo Montero, José A. Vilar (2014). TSclust: An R Package for Time Series
  Clustering. Journal of Statistical Software, 62(1), 1-43. URL
  http://www.jstatsoft.org/v62/i01/.
  
  Chouakria, A. D., & Nagabhushan, P. N. (2007). Adaptive dissimilarity index for measuring time series proximity. Advances in Data Analysis and Classification, 1(1), 5–21. http://doi.org/10.1007/s11634-006-0004-6
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}

\seealso{
To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.
}

\examples{

# The objects example.series1 and example.series2 are two 
# numeric series of length 100.

data(example.series1)
data(example.series2)

# For information on their generation and shape see 
# help page of example.series.

help(example.series)

# Calculate the first correlation based distance between the series using the default 
# parameters.

CortDistance(example.series1, example.series2)



}
