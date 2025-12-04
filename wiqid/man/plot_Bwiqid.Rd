\name{plot.Bwiqid}
\alias{plot.Bwiqid}
\title{
Plot method for objects of class 'Bwiqid'
}
\description{
Method to display a plot showing the posterior probability distribution of one of the parameters of interest.
}
\usage{

\method{plot}{Bwiqid}(x, which=NULL, credMass=0.95,
           ROPE=NULL, compVal=NULL, showCurve=FALSE,
           showMode=FALSE, shadeHDI=NULL,  ...)
}
\arguments{
  \item{x}{
an object of class \code{Bwiqid}.
}
  \item{which}{
character: indicates which parameter to plot. If NULL and \code{x} has a \code{defaultPlot} attribute, that parameter is plotted; otherwise the parameter in column 1 is plotted.
}
   \item{credMass}{
the probability mass to include in credible intervals; NULL suppresses plotting.
}
  \item{ROPE}{
a two element vector, such as \code{c(-1, 1)}, specifying the limit of the ROPE on the estimate; see Details.
}
  \item{compVal}{
a value for comparison with the parameter.
}
  \item{showCurve}{
logical: if TRUE, the posterior density will be represented by a kernel density function instead of a histogram.
}
  \item{showMode}{
logical: if TRUE, the mode of the posterior density will be shown instead of the mean.
}
  \item{shadeHDI}{
specifies a colour to shade the area under the curve corresponding to the HDI; NULL for no shading. Ignored if \code{showCurve = FALSE}. Use\code{colours()} to see a list of possible colours.
}
  \item{...}{
other graphical parameters.
}
}
\details{
The posterior distribution is shown as a histogram or density curve (if \code{showCurve = TRUE}), together with the Highest Density Interval. A ROPE and comparison value are also shown if appropriate.

The probability that a parameter precisely zero (or has any other point value) is zero. More interesting is the probability that the difference from zero may be too small to matter. We can define a region of practical equivalence (ROPE) around zero, and obtain the posterior probability that the true value lies therein.
}
\value{
Returns an object of class \code{histogram} invisibly. Used mainly for the side effect.
}
\references{
Kruschke, J. K. 2013. Bayesian estimation supersedes the \emph{t} test. \emph{Journal of Experimental Psychology: General} 142(2):573-603. doi: 10.1037/a0029146
}
\author{
Mike Meredith, adapted from code by John Kruschke.
}

\seealso{
\code{\link{postPlot}}.
}
\examples{
# See examples in dippers.
}
\keyword{hplot}
\keyword{methods}
