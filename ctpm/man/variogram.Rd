\name{variogram}
\alias{variogram}
\encoding{UTF-8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate an empirical variogram from phylogenetic data.}
\description{
This function calculates the empirical variogram of phylogenetic data for visualizing stationary (time-averaged) autocorrelation structure.
One of two weighting algorithms can be used.
}
\usage{
variogram(data, phylo, weights = "IID", complete = FALSE, time.units = "Ma", 
          trait.units = NULL,  progress = TRUE, algorithm = "GMM")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A vector of continuous species trait data. This vector needs to be of the same length and same order as \code{phylo$tip.label}.}
  \item{phylo}{An object of class 'phylo'.}
  \item{weights}{The weights to apply when calculating the semi-variances. Can be one of \code{"IID"}, or \code{"BM"}. Defaults to \code{"IID"}.}
  \item{time.units}{A character string defining the units of the branch lengths. Defaults to \code{"Ma"}.}
  \item{trait.units}{A character string defining the units of the trait being analysed. Defaults to uniteless.}
  \item{complete}{A logical value indicating whether the semi-variance is to be calculated across all possible lags. Defalts to \code{FALSE}.}
  \item{algorithm}{A character string defining the algorithm to apply when calculating the time-lag bins. Can be one of \code{"kmeans"}, or \code{"GMM"}.}
  \item{progress}{A logical value indicating whether to include a progress bar.}
  }

\details{
\describe{
\item{\code{weights}}{The weights for the semi-variance are calculated based on an assumption about the form of the the correlation matrix. If the phylogenetic process is Independent and Identically Distributed (IID), then it is sufficient to consider a correlation matrix, where the diagonal is 1 and the off-diagonal is \eqn{1/4} if species pair \eqn{(i,j)} and \eqn{(k,l)} share one species in common and 0 otherwise.

If the phylogenetic process is BM, then it is sufficient to consider a correlation matrix where the diagonal is 1 and the off-diagonal is the squared proportion of time lag \eqn{\tau} during which the backward-in-time-forward-in-time tip-branch-tip trajectories correspond to the same species.}

\item{\code{complete}}{If calculating all pairwaise phylogenetic distances results in highly irregular time series (which is usually the case), it is more useful to coarsen the variogram. This is the case when \code{complete = FALSE}. Species are binned across lags, with the number of lags estimated using either \code{kmeans} or Gaussian Mixture Modelling \code{GMM} clustering with n classes = \eqn{\sqrt(N)}.}
}
}

\value{
Returns a variogram object (class variogram) which is a dataframe containing the time-lag, \code{lag}, the semi-variance estimate at that lag, \code{SVF}, and the degrees of freedom on the estimated semi-variance \code{DOF}.
}
\references{
Noonan, M. J., Fagan, W. F., and Fleming C. H. (2021) ``A semi-variance approach to visualising phylogenetic autocorrelation''. Methods in Ecology and Evolution, in press.
}

\author{ M. J. Noonan, C. H. Fleming.}

\note{ Can be slow on very large phylogenies.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{vignette("variogram", package = "ctpm")}, \code{\link{plot.variogram}}, \code{\link[ctmm]{\%#\%}}, \code{\link[ClusterR]{KMeans_rcpp}}, \code{\link[ClusterR]{GMM}}.}

\examples{
#Load package and data
library(ctpm)
data("moid_traits")
data("musteloids")

#Extract the trait of interest from the full dataset
SSD <- moid_traits$SSD

#Calculate variogram
SVF <- variogram(SSD, musteloids)

#Plot the variogram
plot(SVF)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
