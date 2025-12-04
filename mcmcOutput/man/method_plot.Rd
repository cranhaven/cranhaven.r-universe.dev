\name{plot.mcmcOutput}
\alias{plot.mcmcOutput}
\alias{postPlot}
\title{
Graphic display of marginal posterior probability distributions
}
\description{
Plot the posterior probability distribution(s) for the nodes of a \code{mmcmcOutput} object. \code{postPlot} is equivalent to \code{plot(mcmcOutput(object))}.

\emph{Note the new argument \code{center} with options to display the mean, median or mode; this replaces the \code{showMode} argument. The argument \code{CRImass} replaces \code{credMass}.}
}
\usage{
\method{plot}{mcmcOutput}(x, params, layout=c(3,3),
    center = c("mean", "median", "mode"), CRImass=0.95,
    compVal = NULL, ROPE = NULL, HDItextPlace = 0.7,
    showCurve = FALSE, shadeHDI = NULL, ...)


postPlot(object, params, layout=c(3,3),
    center = c("mean", "median", "mode"), CRImass=0.95,
    compVal = NULL, ROPE = NULL, HDItextPlace = 0.7,
    showCurve = FALSE, shadeHDI = NULL, ...)
}
\arguments{
  \item{x}{
An object of class \code{mcmcOutput}.
}
  \item{object}{
An object of any class that can be coerced to class \code{mcmcOutput}.
}
 \item{params}{
An optional vector of column numbers or names; names are partially matched, so \code{params="alpha"} will match all of \code{alpha, alpha0, alpha[1], alphanew}; negative indices indicate parameters to exclude, positive and negative indices cannot be mixed.
}
  \item{layout}{
a length-2 vector with the maximum number of rows and columns to display in the plotting frame.
}
  \item{center}{
the statistic to use to represent the central tendency.
}
  \item{CRImass}{
the probability mass to include in credible intervals; set this to NULL to suppress plotting of credible intervals.
}
  \item{compVal}{
a single value for comparison with those plotted; the same value will be used for all the plots.
}
  \item{ROPE}{
a two element vector, such as \code{c(-1, 1)}, specifying the limits of the Region Of Practical Equivalence; the same value will be used for all the plots.
}
  \item{HDItextPlace}{
a value in [0,1] that controls the horizontal position of the labels at the ends of the HDI bar.
}
  \item{showCurve}{
logical: if TRUE, the posterior density will be represented by a kernel density function instead of a histogram.
}
  \item{shadeHDI}{
specifies a colour to shade the area under the curve corresponding to the HDI; NULL for no shading. Ignored if \code{showCurve = FALSE}. Use \code{colours()} to see a list of possible colours.
}
  \item{\dots}{
graphical parameters and the \code{breaks} parameter for the histogram.
}
}
\details{
The data are plotted either as a histogram (above) or, if \code{showCurve = TRUE}, as a fitted kernel density curve (below). The mean, median or mode of the distribution is displayed, depending on the parameter \code{center}. The Highest Density Interval (HDI) is shown as a horizontal bar, with labels for the ends of the interval.

\figure{plotPost1.jpg} \cr
\cr
\figure{plotPost2.jpg}

If a comparison value (\code{compVal}) is supplied, this is shown as a vertical green dotted line, together with the probability mass below and above this value. If values for a ROPE are supplied, these are shown as dark red vertical dashed lines, together with the percentage of probability mass within the ROPE.
}
\value{
Returns nothing. Used for its plotting side-effect.
}
\author{
Mike Meredith, based on code by John Kruschke.
}

\seealso{
For details of the HDI calculation, see \code{\link{hdi}}.
}
\examples{
# Use the example data set
data(mcmcListExample)
mco <- mcmcOutput(mcmcListExample)
plot(mco)
plot(mco, "p")   # plots p[1,1], p[2,2] and psi
plot(mco, "p[")  # plots p[1,1] and p[2,2], not psi

# Generate some data
normal <- rnorm(1e5, 2, 1)
postPlot(normal)
postPlot(normal, col='wheat', border='magenta')
postPlot(normal, CRImass=0.8, compVal=0, ROPE=c(-0.2,0.2),
    xlab="Response variable")
postPlot(normal, center="mode", showCurve=TRUE, compVal=5.5)

# For integers:
integers <- rpois(1e5, 12)
postPlot(integers)

# A severely bimodal distribution:
bimodal <- c(rnorm(1e5), rnorm(5e4, 7))
postPlot(bimodal)                  # A valid 95% CrI, but not HDI
postPlot(bimodal, showCurve=TRUE)  # Correct 95% HDI
postPlot(bimodal, showCurve=TRUE, shadeHDI='pink')

}
\keyword{hplot}