\name{concprob}
\alias{concprob}
\title{Pairwise empirical and extremal concurrence probabilities}
\description{
  This function computes the pairwise empirical or the pairwise extremal
  concurrence probability estimates.}
\usage{
concprob(data, coord, fitted, n.bins, add = FALSE, xlim = c(0,
max(dist)), ylim = c(min(0, concProb), max(1, concProb)), col = 1:2,
which = "kendall", xlab, ylab, block.size = floor(nrow(data)^(1/3)),
plot = TRUE, compute.std.err = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{fitted}{An object of class maxstab - usually the output of the
    \code{\link{fitmaxstab}} function. May be missing.}
  \item{n.bins}{The number of bins to be used. If missing, pairwise
    F-madogram estimates will be computed.}
  \item{xlim,ylim}{A numeric vector of length 2 specifying the x/y coordinate
    ranges.}
   \item{col}{The colors used for the points and optionnaly the fitted
    curve.}
  \item{which}{A character string specifying which estimator should be
    used. Should be one of "emp" (empirical), "boot" (bootstrap version)
    and "kendall" (kendall based).}
  \item{xlab,ylab}{The labels for the x/y-axis (may be missing).}
  \item{add}{Logical. If \code{TRUE}, the plot is added to the current
    figure; otherwhise (default) a new plot is computed.}
  \item{block.size}{Integer specifying the block size for the empirical
    and bootstrap estimator.}
  \item{plot}{Logical. If \code{TRUE} (default) a plot is produced.}
  \item{compute.std.err}{Logical. If \code{TRUE}, standard errors are
    estimated using a jackknife procedure. It is currently only
    available for the Kendall estimator.}
  \item{\dots}{Additional options to be passed to the \code{\link{plot}}
    function.}
}
%\details{
%}
\value{
  This function returns invisibly a matrix containing the pairwise
  distances and the concurrence probability estimates.
}
\references{
  Dombry, C., Ribatet, M. and Stoev, S. (2017) Probabilities of
  concurrent extremes. \emph{To appear in JASA}
}
\author{Mathieu Ribatet}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{fmadogram}}, \code{\link{lmadogram}}
}
\examples{
n.site <- 25
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
n.obs <- 100
data <- rmaxstab(n.obs, locations, cov.mod = "whitmat", nugget = 0, range = 1,
smooth = 1.75)

##Compute the F-madogram
concprob(data, locations)

##Compare the F-madogram with a fitted max-stable process
fitted <- fitmaxstab(data, locations, "whitmat", nugget = 0)
concprob(fitted = fitted)
}
\keyword{hplot}
