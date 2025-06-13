\name{plot.vg}
\alias{plot.vg}
\encoding{UTF-8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting methods for variogram objects.}
\description{
Produces plots of varigram objects (semi-variance vs. time lag) and model semi-variance functions, with approximate confidence intervals around the semi-variance estimates.
}
\usage{
\S3method{plot}{vg}(x, CTPM = NULL, col="black", col.CTPM = "red", fraction = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A \code{variogram} object calculated using \code{\link{variogram}}.}
  \item{CTPM}{A \code{ctpm} evolutionary model object in the same format as the output of \code{ctpm.fit}.}
  \item{col}{Color for the empirical variogram. Can be an array.}
  \item{col.CTPM}{Color for the model. Can be an array.}
  \item{fraction}{The proportion of the variogram object, \code{variogram}, that will be plotted.}
  \item{\dots}{Additional \code{\link[ctmm]{plot.variogram}} function parameters.}
  }

\details{
For highly irregularly phylogenetic distances with few species, it may be useful to set \code{complete = FALSE} to coarsen the variogram. When this is the cases, species are binned across lags, with the number of lags estimated using either \code{kmeans} or Gaussian Mixture Modelling \code{GMM} clustering with n classes = \eqn{\sqrt(N)}.
}

\value{Returns a plot of semi-variance vs. time lag, with the empirical variogram in black and the \code{ctpm} semi-variance function in red if specified.}

\references{
Noonan, M. J., Fagan, W. F., and Fleming C. H. (2021) ``A semi-variance approach to visualising phylogenetic autocorrelation''. Methods in Ecology and Evolution, in press.
}

\author{ M. J. Noonan, C. H. Fleming.}

\note{The errors of the empirical variogram are correlated. Smooth trends are not necessarily significant.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{vignette("variogram", package = "ctpm")}, \code{\link{variogram}}, \code{\link{ctpm.fit}}. }

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
