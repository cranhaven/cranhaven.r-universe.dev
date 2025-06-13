\name{ctpm.fit}
\alias{ctpm.fit}
\encoding{UTF-8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit models to phylogenetic data.}
\description{
This function is currently a wrapper for methods implemented in the package \code{\link{slouch}}.
}
\usage{
ctpm.fit(data, phylo, model = NULL, time.units = "Ma")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A vector of continuous species trait data. This vector needs to be of the same length and same order as phylo$tip.label. }
  \item{phylo}{An object of class 'phylo'.}
  \item{model}{The model to fit. Can be one of \code{"IID"}, \code{"BM"}, or \code{"OU"}.}
  \item{time.units}{A character defining the units of the edge lengths of the phylogentic tree. Defaults to "Ma"}
  }

\details{
Fits evolutionary models to species trait data using the methods implemented in the package \code{\link{slouch}} and returns a model fit object (class ctmm) that is used for plotting a fitted model against an emprical variogram.
}

\value{
Returns a model fit object (class ctmm) that is used for plotting a fitted model against an emprical variogram. 
}
\references{
Noonan, M. J., Fagan, W. F., and Fleming C. H. (2021) ``A semi-variance approach to visualising phylogenetic autocorrelation''. Methods in Ecology and Evolution, in press.
}

\author{ M. J. Noonan, C. H. Fleming.}

\note{ Running \code{summary} on the fitted model is not currently meaningful.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{vignette("variogram", package = "ctpm")}, \code{\link{plot.variogram}}, \code{\link[ctmm]{\%#\%}}. }

\examples{
#Load package and data
library(ctpm)
data("moid_traits")
data("musteloids")

#Extract the trait of interest from the full dataset
SSD <- moid_traits$SSD

#Calculate variogram
SVF <- variogram(SSD, musteloids)

#Fit the model
FIT <- ctpm.fit(SSD, musteloids, model = "BM")


#Plot the variogram and fitted model
plot(SVF, FIT)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
