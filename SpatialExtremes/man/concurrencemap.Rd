\name{concurrencemap}
\alias{concurrencemap}
\title{Maps of concurrence probabilities/expected concurrence cell area}
\description{
  This function produces maps for concurrence probabilities or expected
  concurrence cell areas.}
\usage{
concurrencemap(data, coord, which = "kendall", type = "cell", n.grid =
100, col = cm.colors(64), plot = TRUE, plot.border = NULL,
compute.std.err = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{which}{A character string specifying which estimator should be
    used. Should be one of "emp" (empirical), "boot" (bootstrap version)
    and "kendall" (kendall based).}
  \item{type}{Either "cell" for cell areas or a integer between 1 and
    the number of locations specifying which site should be used as
    reference location---see Details.}
  \item{n.grid}{Integer specifying the size of the prediction grid.}
   \item{col}{The colors used to produce the map.}
   \item{plot}{Logical. If \code{TRUE} (default), a map is produced;
     otherwise results are invisibly returned.}
  \item{plot.border}{The name of an R function that can be used to plot
    the border of the study region. If \code{NULL}, no border are
    plotted.}
  \item{compute.std.err}{Logical. If \code{TRUE}, a map of standard
    errors is also produced. It is currently only available for
    concurrence probability maps.}
  \item{\dots}{Additional options to be passed to the
    \code{\link{image}} function.}
}
%\details{
%}
\value{
  This function returns invisibly a list with the x and y coordinates
  and the corresponding values for the estimated concurrence
  probabilities or expected concurrence cell area.
}
\references{
  Dombry, C., Ribatet, M. and Stoev, S. (2015) Probabilities of
  concurrent extremes. \emph{Submitted}
}
\author{Mathieu Ribatet}

\seealso{
  \code{\link{concprob}}
}
\examples{
##require(maps) ## <<-- to plot US borders
data(USHCNTemp)
coord <- as.matrix(metadata[,2:3])

## Subset the station to have a fast example
n.site <- 30
chosen.site <- sample(nrow(coord), n.site)
coord <- coord[chosen.site,]
maxima.summer <- maxima.summer[,chosen.site]

## Define a function to plot the border
border <- function(add = FALSE) maps::map("usa", add = add)

par(mar = rep(0, 4))
## Produce a pairwise concurrence probability map w.r.t. station number 15
concurrencemap(maxima.summer, coord, type = 15, plot.border = border,
 compute.std.err = TRUE)

## Produce the expected concurrence cell area
concurrencemap(maxima.summer, coord, plot.border = border)
}
\keyword{hplot}
