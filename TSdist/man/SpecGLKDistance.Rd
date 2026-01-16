\name{SpecGLKDistance}
\alias{SpecGLKDistance}

\title{
Dissimilarity based on the Generalized Likelihood Ratio Test
}
\description{
The dissimilarity of two numerical series of the same length is calculated based on an adaptation of the generalized likelihood ratio test.
}

\usage{
SpecGLKDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{...}{
   Additional parameters for the function. See  \code{\link[TSclust]{diss.PER}} for more 
  information.
}
}

\details{

This function simply intends to be a wrapper for the \code{\link[TSclust]{diss.SPEC.GLK}} function of package \pkg{TSclust}. However, in the 1.2.3 version of the \pkg{TSclust} package we have found an error in the call to this function. As such, in this version, the more general \code{\link[TSclust]{diss}} function, designed for distance matrix calculations of time series databases, is used to calculate the spec.glk distance between two series. Once this bug is fixed in the original package, we will update our call procedure.

%\bold{Note:} The negative definiteness of this distance measure is not explicitly mentioned in the literature, to the best of our knowledge, and so is not assured. As such, when using it within kernel based classifiers such as Support Vector Machines or Gaussian Processes (i.e. by inserting this distance in the Gaussian RBF kernel) the user should make sure that the obtained Gram matrix is positive semi-definite. More information and references to some solutions to this problem can be found in (Pree et al. 2014).
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

# Calculate the ar.mah distance between the two series using
# the default parameters. 

SpecGLKDistance(example.series1, example.series2)

}
