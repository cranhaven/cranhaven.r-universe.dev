\name{plot.compare}
\alias{plot.compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Summary and Diagnostic Plots for \code{compare} Objects
}
\description{
	This is a \code{\link{plot}} method for objects of class \code{compare}. It automates the creation of four summary and diagnostic plots for \code{compare} objects.
}
\usage{
	\method{plot}{compare}(x, y, ..., which = "cdf", stat = NULL, hlines = NULL, curve_args = NULL,
	confband_args = FALSE, density_args = NULL, poly = NULL, legend = NULL,
	qqline = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	Object of class \code{compare}.
}
  \item{y}{
	Optional object of class \code{compare}.
}
  \item{\dots}{
	Additional parameters passed to a plotting function depending on the
	value of \code{which}: to \code{\link{plot}} for "\code{cdf}" and
	"\code{dens}"; to \code{\link{qqplot}} for "\code{qq}" and "\code{qqp}".
}
  \item{which}{
	Character string. Acceptable values are "\code{cdf}", "\code{dens}", "\code{qq}", and "\code{qqp}":
	\itemize{
	  \item "\code{cdf}" produces a plot of the estimated cumulative distribution
	  functions of the two vectors of statistics being compared.
	  \item "\code{dens}" produces a plot of the estimated density functions of
	  the two vectors of statistics being compared.
	  \item "\code{qq}" produces a quantile--quantile plot comparing the two
	  vectors of statistics.
	  \item "\code{qqp}" produces a quantile--quantile plot comparing the p-values
	  with uniform quantiles. (This is not available when \code{y} is present.)
	}
}
  \item{stat}{
  Character string, specifies the statistic to plot.
}
  \item{hlines}{
	An optional list of arguments to pass to \code{\link{abline}}, used to create the horizontal dashed lines when \code{which} is "\code{cdf}". Setting equal to \code{FALSE} prevents the call, and no lines are drawn.
}
  \item{curve_args}{
	An optional list used to pass arguments to \code{\link{lines}} (not \code{\link{curve}}!), used to create the curves when \code{which} is "\code{cdf}" or "\code{dens}". It can have two special named arguments, \code{obs} and \code{mcsim}, whose values must be lists. Those lists contain arguments passed to the calls to \code{\link{lines}} for plotting the curves for the observed and simulated statistics, respectively. Any other elements are passed to both calls.
}
  \item{confband_args}{
	An optional list of values that control the calculation and plotting of confidence bands when \code{which} is "\code{cdf}" or "\code{dens}". Any of the following named elements are allowed.
		\itemize{
	  \item \code{w}: Numeric; the sequence of points on which to evaluate the confidence band. By default, the sequence is \code{seq(from = min(x) + buffer, to = max(x) - buffer, length.out = m)}, where \code{x} is the vector of values of the statistic in question, and \code{buffer} is explained below.
	  \item \code{m}: Integer; the length of \code{w}, used only when \code{w} is \code{NULL}. The default value is 100.
	  \item \code{batch_len}: Integer; the batch length for the algorithm. The default value is 50.
	  \item \code{N}: Integer; the number of multivariate \eqn{t} samples to use in the simulation.
	  \item \code{conf.level}: Numeric; the desired confidence level.
	  \item \code{buffer}: Numeric; the proportion of either side of the range of data to ignore when defining \code{w}, used only when \code{w} is \code{NULL}.
	  \item \code{curve_args}: An optional list of arguments passed to \code{\link{lines}} (again, not \code{\link{curve}}!), used to create the boundaries of the confidence band. It can have two special named arguments, \code{obs} and \code{mcsim}, which function in the same way as the corresponding elements of the \code{curve_args} argument described above.
	  \item \code{polygon_args}: An optional list of arguments passed to \code{\link{polygon}}, used to shade the confidence region. Setting equal to \code{FALSE} prevents the call, and no shading is done.
	  \item \code{shade_col}: This provides a shortcut to the \code{col} argument of \code{\link{polygon}} to change the color of the shaded region.
	}
	Setting equal to \code{FALSE} prevents calculation and plotting of the band.
}
  \item{density_args}{
	An optional list of arguments passed to \code{\link{density}} when \code{which} is "\code{dens}", which calculates the points used to plot the density curves. The list can have two special named elements, \code{obs} and \code{mcsim}, which function in the same way as the corresponding elements of the \code{curve_args} argument described above.
}
  \item{poly}{
	An optional list of arguments passed to \code{\link{polygon}} when \code{which} is "\code{dens}", which shades the area under the density curves. The list can have two special named elements, \code{obs} and \code{mcsim}, which modify the shadings for their respective curves, analogous to their behavior in the \code{curve_args} argument. When \code{poly} is equal to \code{FALSE}, no call is made, and therefore no shading is done.
}
  \item{legend}{
  An optional list of arguments passed to \code{\link{legend}} when \code{which} is "\code{cdf}" or "\code{dens}". When equal to \code{FALSE}, no call is made, and therefore no legend is created.
}
  \item{qqline}{
  An optional list of arguments passed to \code{\link{abline}} when \code{which} is "\code{qq}" or "\code{qqp}". By default, this plots the line \eqn{y=x}. When equal to \code{FALSE}, no call is made, and therefore no line is plotted.
}
}
\details{
	This function produces a plot of a type specified by \code{which}. The values
	plotted depend on whether or not \code{y} is present and the value of
	\code{which}. When \code{y} is present, the plots compare the observed
	statistics in \code{x} and the observed statistics in \code{y}. When \code{y}
	is missing, the plots compare the observed and simulated statistics in
	\code{x}. (The exception is when \code{which} is "\code{qqp}", which is only
	available when \code{y} is missing.)
	
	When \code{which} is "\code{cdf}" or "\code{dens}", the plotting region and
	associated labels, tick marks, etc., are created by an initial call to
	\code{\link{plot}}. The curves themselves are drawn with \code{\link{lines}}.
	The arguments specified in \code{...} are passed to the initial call to
	\code{\link{plot}}.
}
\value{
	The values used to create the curves (or points, in the case of a Q--Q plot) are returned invisibly. The details depend on the value of \code{which}:
	\itemize{
	  \item \code{cdf}: A list with two or four elements, all lists. The first two sub-lists contain the \eqn{x}- and \eqn{y}-values cdf curves. If confidence bands are plotted, then two additional elements are included with output from the confidence band calculations, including elements \code{w}, \code{cb_lower}, and \code{cb_upper}, which contain, respectively, the \eqn{x}-coordinates for both the upper and lower bounds of the band, the \eqn{y}-coordinates for the lower band, and the \eqn{y}-coordinates for the upper band.
	  \item \code{dens}: A list with two or four elements, all lists. The first two sub-lists contain \eqn{x}- and \eqn{y}-values for the density curves. If confidence bands are plotted, then two additional sub-lists are supplied, with contents identical to what is described for "\code{cdf}".
	  \item \code{qq}, \code{qqp}: The output of \code{\link{qqplot}}.
	}
  For "\code{cdf}" and "\code{dens}", the names of the elements of the returned list depend on whether or not a value for the argument \code{y} was supplied.
}
\author{
	Jesse Miller
}
\references{
Flegal, James M. et al. \strong{Simultaneous confidence bands for (Markov chain) Monte Carlo simulations}, forthcoming.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{distfreereg}}, \code{\link{compare}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
