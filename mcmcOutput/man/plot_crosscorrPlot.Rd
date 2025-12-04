\name{crosscorrPlot}
\alias{crosscorrPlot}
\title{
Plot image of correlation matrix
}
\description{
Displays graphically the lower triangle of the correlation matrix among the columns of the input.
}
\usage{

crosscorrPlot(x, params=NULL, col, addSpace=c(0,0), ...)

}
\arguments{
  \item{x}{
An object with MCMC chain values, of any class that can be coerced to \code{mcmcOutput}.
}
  \item{params}{
An optional vector of column numbers or names; names are partially matched, so \code{params="alpha"} will match all of \code{alpha, alpha0, alpha[1], alphanew}; negative indices indicate parameters to exclude, positive and negative indices cannot be mixed.
}
  \item{col}{
The colours to use to code the correlations; default is a blue-yellow-red ramp; NA correlations appear as white boxes.
}
  \item{addSpace}{
A length-2 vector to add extra white space right and above the main display, eg, to provide extra space for long parameter names; units are the width of one box of the display. Unlike \code{mar}, this inserts space left of the legend and below the title.
}
  \item{\dots}{
Additional graphical parameters, see Details.
}
}
\details{
The usual graphical parameters can be added in the \dots argument. Note the following:

 * \code{mar} : a vector of length 4 specifying the width of the margins below, left, above and right of the main plot; you will need to increase the margins to insert \code{xlab} or \code{ylab}; default \code{c(1,1,5,4)}; see the entry for \code{mar} at \code{\link{par}}.

 * \code{cex.axis} : controls the size of the parameter names, default 1.2.

 * \code{srt} : controls the rotation of the parameter names, 0 = horizontal, 90 = vertical, default 45.

 * \code{offset} : controls the distance of the start of the parameter names from the corner of the box in units of box width, default 0.2.

 * \code{tcl} : the length of the tick marks next to the parameter names in units of box widths, default 0.1.

 * \code{lwd.ticks} : line width for the tick marks, default 1.

 * \code{legendAsp} : aspect ratio for the legend, default 0.1.

}
\value{
Returns the correlation matrix invisibly.
}
\author{
Mike Meredith
}

\examples{
# Create a data frame of fake MCMC output:
mu0 <- rnorm(3000)         # normal, mean zero
mu10 <- rnorm(3000, rep(9:11, each=1000), 1) + mu0*0.5
  # approx normal, mean 10, correlated with mu0
fake <- data.frame(
  mu0 = mu0,
  mu10 = mu10,
  sigma=rlnorm(3000),        # non-negative, skewed
  prob = plogis(1-mu0),      # probability, central mode, neg. correlation with mu0
  prob0 = rbeta(3000, 1,2),  # probability, mode = 0
  N = rpois(3000, rep(c(24, 18, 18), each=1000)),
                             # large integers (no zeros), poor mixing
  n = rpois(3000, 2),        # small integers (some zeros)
  const1 = rep(1, 3000))     # all values = 1
str(fake)

tmp <- crosscorrPlot(fake)
round(tmp, 2)
crosscorrPlot(fake, main="Isn't this a really cool plot?")
crosscorrPlot(fake, main="A subset of parameters", params=c("mu", "prob", "N"))
crosscorrPlot(fake, main="Leave out 'sigma'", params=-3)
crosscorrPlot(fake, main="Just a few colours", col=c("blue","skyblue","pink","red"))
names(fake)[5] <- "A_parameter_with_a_very_long_name"
crosscorrPlot(fake, main="Is there room?")
crosscorrPlot(fake, main="With addSpace=c(2,0)", addSpace=c(2,0))
}
\keyword{hplot}