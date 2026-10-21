\name{plot.maxstab}
\alias{plot.maxstab}
\title{
Model checking of a fitted max-stable model
}
\description{
This function produces several plots to assess the goodness of fit of a
fitted max-stable model.
}
\usage{\method{plot}{maxstab}(x, ..., sites)}
\arguments{
  \item{x}{An object of class \code{maxstab}. Most often, this will be
    the output of \code{\link{fitmaxstab}} or \code{\link{lsmaxstab}}.}
  \item{\dots}{Here for compatibility reasons but not yet implemented.}
  \item{sites}{A vector of integer of length 4 specifying the locations
    to be used for the model checking. If missing, locations will be
    choosen randomly.}
}
\value{
  Several diagnostic plots.
}
\details{
  The diagonal plots are return level plots. The lower ones are qq-plots
  (on the Gumbel scale) between observed pairwise maxima for each block,
  e.g. year, and the ones obtained by simulations from the fitted
  model. The upper plot compares the fitted extremal coefficient
  functions to semi-empirical estimates from the F-madogram - see
  \code{\link{fmadogram}}. The two remaining plots are the stations
  locations and a qq-plot of blockwise maxima where the block size is 4.
}
\author{Mathieu Ribatet}
\examples{
n.site <- 20
n.obs <- 50
coord <- matrix(runif(2 * n.site, 0, 10), ncol = 2)
colnames(coord) <- c("lon", "lat")
data <- rmaxstab(n.obs, coord, "powexp", nugget = 0, range = 3, smooth =
1)
fitted <- fitmaxstab(log(data), coord, "powexp", y ~ 1, y ~ 1, y ~ 1,
nugget = 0)
plot(fitted)
}
\keyword{hplot}
