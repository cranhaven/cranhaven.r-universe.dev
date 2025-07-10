\name{denoverplot1}
\alias{denoverplot1}
\title{ Plot Overlaying Densities }

\description{
  Creates a plot containing overlaying kernel density estimates from different MCMC simulations.  This function is used in the \code{denoverplot} function to produce plots of overlaying densities for parameters in common from two different MCMC simulations.
}

\usage{
denoverplot1(\dots, ci = NULL, col = NULL, lty = 1, xlim = NULL,
ylim = NULL, xlab = "", ylab = "Density", main = NULL, style = c("gray",
"plain"), gpar = NULL)
}

\arguments{
  \item{\dots}{ one or more vectors or a list containing one or more vectors to be plotted.}
  \item{ci}{ if non \code{NULL}, plots \code{(100*ci)}\% credible interval limits on the density plots.  The default (\code{NULL}) is not to plot the intervals.}
  \item{col}{ one or more colors for the densities.  Default is \code{mcmcplotsPalette(n)}, where \code{n} is the number of elements in the \dots argument.}
  \item{lty}{ types of lines to plot.}
  \item{xlim}{ limits for the x axis.}
  \item{ylim}{ limits for the y axis.}
  \item{xlab}{ label for the x axis.}
  \item{ylab}{ label for the y axis.}
  \item{main}{ main title for plot.}
  \item{style}{ if "gray", then the plotting region is printed with a gray background, otherwise the default plotting region is used.}
  \item{gpar}{ a list of additional graphical parameters to be passed to the plotting function.  See help for \code{par}.}
}

\value{
  Creates a plot.
}

\author{
  S. McKay Curtis with contributions from Ilya Goldin
}

\seealso{
  \code{\link{denoverplot}}, \code{\link{denplot}}, \code{\link{traplot1}}
}

\examples{
denoverplot1(rnorm(1000), rnorm(1000))
denoverplot1(rnorm(1000, 0.0, 1.0), rnorm(1000, 0.1, 1.0),
             style="plain", col=mcmcplotsPalette(2, type="grayscale"),
             ci=0.95)
denoverplot1(list(rgamma(1000, 1, 1), rgamma(1000, 1, 1)))
}

\keyword{ hplot }
