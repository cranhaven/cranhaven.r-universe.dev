
\name{plot.pendensity}
\alias{plot.pendensity}
\alias{plot}

\title{Plotting estimated penalized densities}
\description{
Plotting estimated penalized densities, need object of class 'pendensity'. 
}
\usage{
\method{plot}{pendensity}(x, plot.val = 1, val=NULL, latt = FALSE, kernel = FALSE, confi = TRUE,
 main = NULL, sub = NULL, xlab = NULL, ylab = NULL, plot.base = FALSE,
 lwd=NULL,legend.txt=NULL,plot.dens=TRUE,...)
}

\arguments{
  \item{x}{object of class pendensity}
  \item{plot.val}{if plot.val=1 the density is plotted, if plot.val=2
  the distribution function of the observation values is plotted, if
  plot.val=3 the distribution function is plotted as function}
  \item{val}{vector of y, at which the estimated density should be calculated. If plot.val=2, the calculated values of distribution are returned and the values are pointed in the distribution function of the observed values.}
  \item{latt}{TRUE/FALSE, if TRUE the lattice interface should be used for plotting, default=FALSE}
  \item{kernel}{TRUE/FALSE, if TRUE a kernel density estimation should be added to the density plots, default=FALSE}
  \item{confi}{TRUE/FALSE, if TRUE confidence intervals should be added to the density plots, default=TRUE }
  \item{main}{Main of the density plot, if NULL main contains settings 'K', 'AIC' and 'lambda0' of the estimation}
  \item{sub}{sub of the density plot, if NULL sub contains settings used base 'base' and used order of B-Spline 'q'}
  \item{xlab}{xlab of the density plot, if NULL xlab contains 'y'}
  \item{ylab}{ylab of the density plot, if NULL ylab contains 'density'}
  \item{plot.base}{TRUE/FALSE, if TRUE the weighted base should be added to the density plot, default=FALSE}
  \item{lwd}{lwd of the lines of density plot, if NULL lwd=3, the confidence bands are plotted with lwd=2}
  \item{legend.txt}{if FALSE no legend is plotted, legend.txt can get a
  vector of characters with length of the groupings. legend.txt works
  only for plot.val=1}
  \item{plot.dens}{TRUE/FALSE, if the estimated density should be plotted. Default=TRUE. Interesting for evaluating densities in values 'val', while this special plot is not needed.}
  \item{...}{further arguments}
}
\details{
Each grouping of factors is plotted. Therefore, equidistant help values are constructed in the support of the response for each grouping of factors. Weighting these help values with knots weights ck results in the density estimation for each grouping of factors. If asked for, pointwise confidence intervals are computed and plotted.
}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}

\value{
If the density function is plotted, function returns two values
\item{help.env}{Contains the constructed help values for the response, the corresponding values for the densities and if asked for the calculated confidence intervals}
\item{combi}{list of all combinations of the covariates}
If additionally the function is called with a valid argument for 'val',
a list returns with
\item{y}{values at which the estimated density has been calculated}
\item{fy}{calculated density values in y}
\item{sd.up.y.val}{the values of the upper confidence interval of y}
\item{sd.down.y.val}{the values of the lower confidence interval of y}

If the empirical distribution function is plotted, the function returns
\item{y}{containing the observed values y}
\item{sum}{containing the empirical distribution of each observation y}
If the theoretical distribution function is plotted, the function returns an environment. For plotting the theoretical distributions, each interval between two knots is evaluated at 100 equidistant simulated points between the two knots considered. These points are saved in the environment with the name "paste("x",i,sep="")" for each interval i, the calculated distribution is save with the name "paste("F(x)",i,sep="")" for each interval i. For these points, the distribution is calculated. 
}
\note{
For plotting the density and e.g. the empirical distributions, use e.g. 'X11()' before calling the second plot to open a new graphic device.
}
\examples{
y <- rnorm(100)
test <- pendensity(y~1)
plot(test)

#distribution
plot(test,plot.val=2)
}
\keyword{aplot}
