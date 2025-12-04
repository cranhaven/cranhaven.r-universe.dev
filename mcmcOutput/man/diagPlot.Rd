\name{diagPlot}
\alias{diagPlot}
\alias{tracePlot}
\alias{densityPlot}
\alias{acfPlot}
\title{
Diagnostic graphics for class \code{mcmcOutput}
}
\description{
Display trace plots and density plots for the chains in the MCMC output. Each chain is plotted with a different colour. %See also \code{\link{crosscorrPlot}}.

}
\usage{
diagPlot(object, params, howMany, chains,
  maxRows=4, RhatBad=1.05, precision=c("MCEpc", "n.eff"), ask=NULL, ...)

tracePlot(object, layout=c(3,3), ask=NULL, ...)
densityPlot(object, layout=c(3,3), ask=NULL, ...)
acfPlot(object, lag.max=NULL, layout=c(3,3), ask=NULL, ...)
}
\arguments{
  \item{object}{
An object of any class with MCMC output that can be coerced to class \code{mcmcOutput}.
}
  \item{params}{
An optional vector of column numbers or names; names are partially matched, so \code{params="alpha"} will match all of \code{alpha, alpha0, alpha[1], alphanew}; negative indices indicate parameters to exclude, positive and negative indices cannot be mixed.
}
  \item{howMany}{
How many draws per chain to plot; if negative, the draws at the end of the chains will be plotted; default is to plot all.
}
  \item{chains}{
Which chains to plot, a numeric vector; default is to plot all.
}
  \item{maxRows}{
Maximum number of rows to display in one window; each row consists of a trace plot
 and a density plot for one parameter.
}
  \item{RhatBad}{
Threshold for Rhat; parameters with \code{Rhat > RhatBad} are highlighted in red.
}
  \item{precision}{
The statistic to use for the precision, displayed above the density plot.
}
  \item{layout}{
a length-2 vector with the maximum number of rows and columns to display in the plotting frame.
}
  \item{lag.max}{
Maximum lag at which to calculate the acf.
}
  \item{ask}{
If \code{TRUE} and the number of parameters to plot is greater than \code{maxRows}, the user will be prompted before the next page of output is displayed. The default is to ask if the plotting device is the screen, not if it is a file.
}
  \item{\dots}{
Additional graphical parameters.
}
}

\value{
Return nothing, used for their plotting side effects.
}
\author{
Mike Meredith
}

\seealso{
\code{\link{crosscorrPlot}}, \code{\link{postPlot}} for a histogram and summary statistics.
}
\examples{
# Create a fake mcmcOutput object:
tmp <- cbind(
  mu0 = rnorm(3000),         # normal, mean zero
  mu10 = rnorm(3000, rep(9:11, each=1000), 1),
                             # normal, mean 10, but poor mixing
  sigma=rlnorm(3000),        # non-negative, skewed
  `prob[1,1]` = rbeta(3000, 4, 4),  # probability, central mode
  `prob[1,2]` = 0.3,         # constant
  `prob[2,1]` = rbeta(3000, 1, 3),  # probability, mode = 0
  N = rpois(3000, rep(c(24, 18, 18), each=1000)),
                             # large integers (no zeros), poor mixing
  n = rpois(3000, 2),        # small integers (some zeros)
  allNA = NA,                # all values NA
  someNA = suppressWarnings(log(rnorm(3000, 2, 2))),
                             # some NaNs
  const1 = rep(1, 3000),     # all values = 1
  const3.2 = rep(10/3, 3000))# all values the same but not integer
( fake <- mcmcOutput(tmp, nChains = 3) )
summary(fake)
diagPlot(fake)
diagPlot(fake, params=3:6, main="params = 3:6")
diagPlot(fake, params=c("mu", "prob"), main="params = c('mu', 'prob')")
diagPlot(fake, params=c("mu", "prob"), howMany=200, main="howMany = 200")
diagPlot(fake, params=c("mu", "prob"), howMany=50, main="howMany = 50")
diagPlot(fake, params=c("mu", "prob"), howMany=-200, main="howMany = -200")
diagPlot(fake, params=c("mu", "prob"), chains=1:2, main="chains = 1:2")
diagPlot(fake, params=c("mu", "prob"), chains=2, main="chains = 2") # 1 chain -> no Rhat

tracePlot(fake, layout=c(2,2))
densityPlot(fake, xlab="value")
acfPlot(fake, lag.max=10, lwd=2)

# Use diagPlot with an mcmc.list object
data(mcmcListExample)
diagPlot(mcmcListExample)
diagPlot(mcmcListExample, main="example", params=1:3, precision="n.eff")

}
\keyword{hplot}