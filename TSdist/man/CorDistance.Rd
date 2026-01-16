\name{CorDistance}
\alias{CorDistance}

\title{
Dissimilarities based on Pearson's correlation
}

\description{
Computes two different distance measure based on Pearson's correlation between a pair of
numeric time series of the same length.
}

\usage{
CorDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{...}{
   Additional parameters for the function. See  \code{\link[TSclust]{diss.COR}} for more 
  information.
}
}

\details{
This is simply a wrapper for the \code{\link[TSclust]{diss.COR}} function of package \pkg{TSclust}. As such, all the functionalities of the \code{\link[TSclust]{diss.COR}} function are also available when using this function. 

%\bold{Note:} From the two variants of correlation based distances that can be calculated with this function, only one (denominated d1 in the documentation of \code{TSclust}) is a metric (Golay et al., 1998). As such, this first distance can be used directly within kernel machines, e.g. by inserting it in the Gaussian RBF kernel (Lei and Sun, 2007), because it will provide positive definite Gram matrices, which is a requirement for kernel based classifiers such as Support Vector Machines or Gaussian Processes. When using the second measure, since the negative definiteness is not mentioned explicitly in the literature, we recommend analyzing the Gram matrix before introducing it into kernel based classifiers. More information and some solutions to this problem can be found in (Pree, 2014)
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
  
  Golay, X., Kollias, S., Stoll, G., Meier, D., Valavanis, A., & Boesiger, P. (1998). A new correlation-based fuzzy logic clustering algorithm for FMRI. Magnetic Resonance in Medicine, 40(2), 249–260.
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

# Calculate the first correlation based distance between the series.

CorDistance(example.series1, example.series2)

# Calculate the second correlation based distance between the series
# by specifying \eqn{beta}.

CorDistance(example.series1, example.series2, beta=2)

}
