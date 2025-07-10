\name{corplot}
\alias{corplot}
\title{ Plot a Correlation Matrix }

\description{
  Creates an image plot of a correlation matrix where colors of different shades represent differing levels of correlation.  
}

\usage{
corplot(mat, col = mcmcplotsPalette(11, "sequential"), outline = TRUE,
greek = FALSE, legend.scale = 0.75, mar=c(5, 4, 1, 1) + 0.1, \dots)
}

\arguments{
  \item{mat}{ correlation matrix.}
  \item{col}{ colors to be used in the plot.}
  \item{outline}{ logical indicating whether outlines of image squares should be drawn. }
  \item{greek}{if \code{TRUE}, the names of greek letters in the \code{labels} will be displayed as greek characters on the plot.}
  \item{legend.scale}{ scales the height of the legend with respect to the height of the plot.  Default is 0.75 which makes the legend 3 quarters as tall as the plot.}
  \item{mar}{ graphical parameter \code{mar}.  See documentation for \code{par}.}
  \item{\dots}{ further arguments passed to the plotting function. }
}

\details{
  One possible use of this function is to plot the correlation between posterior draws of an MCMC run.  Patterns in the plot can aid in constructing a more efficient blocking structure for an MCMC algorithm, where highly correlated parameters should be placed in the same MCMC update block.  None.
}

\value{
  Creates a plot.
}

\author{ S. McKay Curtis }

\seealso{ \code{image} }

\examples{
Rho <- matrix(c(
 1.00,  0.35, -0.65, -0.66,  0.46,  0.42,
 0.35,  1.00, -0.69, -0.64,  0.40, -0.06,
-0.65, -0.69,  1.00,  0.70, -0.57, -0.11,
-0.66, -0.64,  0.70,  1.00, -0.15, -0.10,
 0.46,  0.40, -0.57, -0.15,  1.00,  0.18,
 0.42, -0.06, -0.11, -0.10,  0.18,  1.00), 6, 6)
dimnames(Rho) <- list(paste("rho[", 1:6, "]", sep=""), paste("rho[", 1:6, "]", sep=""))
corplot(Rho)
corplot(Rho, greek=TRUE)
}
\keyword{ hplot }
