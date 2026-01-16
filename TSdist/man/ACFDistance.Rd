\name{ACFDistance}
\alias{ACFDistance}

\title{
Autocorrelation-based Dissimilarity
}
\description{
Computes the dissimilarity between a pair of numeric time series based on their 
estimated autocorrelation coefficients.
}

\usage{
ACFDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{...}{
   Additional parameters for the function. See  \code{\link[TSclust]{diss.ACF}} for more 
  information.
}
}

\details{
This is simply a wrapper for the \code{\link[TSclust]{diss.ACF}} function of package \pkg{TSclust}. As such, all the functionalities of the \code{\link[TSclust]{diss.ACF}} function are also available when using this function. 

%\bold{Note:} This distance measure is a metric (Galeano and Pella, 2000). This means that it will provide positive definite Gram matrices and, thus, it can be used directly within kernel machines e.g. by inserting it in the Gaussian RBF kernel (Lei and Sun, 2007).

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
  
  Galeano, P., & Pella, D. (2000). Multivariate Analysis in Vector Time Series Pedro Galeano and Daniel Pella. Resenhas, the Journal of the Institute of Mathematics and Statistics of the University of Sao Paolo, 4, 383–403.
  
  Lei, H., & Sun, B. (2007). A Study on the Dynamic Time Warping in Kernel Machines. In 2007 Third International IEEE Conference on Signal-Image Technologies and Internet-Based System (pp. 839–845). 
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}



\seealso{

To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.

}

\examples{

# The objects example.series3 and example.series4 are two 
# numeric series of length 100 and 120 contained in the 
# TSdist package. 

data(example.series3)
data(example.series4)

# For information on their generation and shape see 
# help page of example.series.

help(example.series)

# Calculate the autocorrelation based distance between the two series using
# the default parameters:

ACFDistance(example.series3, example.series4)

}
