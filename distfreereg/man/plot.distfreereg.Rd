\name{plot.distfreereg}
\alias{plot.distfreereg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Summary and Diagnostic Plots for \code{distfreereg} Objects
}
\description{
	This is a \code{\link{plot}} method for objects of class \code{distfreereg}. It automates the creation of three summary and diagnostic plots for \code{distfreereg} objects.
}
\usage{
	\method{plot}{distfreereg}(x, which = "dens", stat = NULL, density_args = NULL, polygon_args = NULL,
	confband_args = NULL, abline_args = NULL, shade_col = rgb(1,0,0,0.5),
	text_args = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	Object of class \code{distfreereg}.
}
  \item{which}{
	Character string. Acceptable values are "\code{dens}", "\code{residuals}", and "\code{epsp}":
	\itemize{
	  \item "\code{dens}" produces a plot of the estimated density curve of the
	  specified statistic.
	  \item "\code{residuals}" produces a plot of the transformed residuals in the
	  order specified by \code{x$res_order}.
	  \item "\code{epsp}" produces a plot of the empirical partial sum process of
	  the (ordered) transformed residuals.
	}
}
  \item{stat}{
	Character vector of length one specifying the name of the statistic to plot
	when \code{which} is "\code{dens}". By default, the first statistic in
	\code{x$observed_stats} is used.
}
  \item{density_args}{
	An optional list of arguments to pass to \code{\link{density}}.
}
  \item{polygon_args}{
	An optional list of arguments to pass to \code{\link{polygon}}, used to shade under the density curve to the right of the value of the observed statistic. Setting equal to \code{FALSE} prevents the call, and no shading is done.
}
  \item{confband_args}{
	An optional list of values that control the calculation and plotting of confidence bands. Any of the following named elements are allowed.
		\itemize{
	  \item \code{w}: Numeric; the sequence of points on which to evaluate the confidence band. By default, the sequence is \code{seq(from = min(x) + buffer, to = max(x) - buffer, length.out = m}, where \code{x} is the vector of values of the statistic in question, and \code{buffer} is explained below.
	  \item \code{m}: Integer; the length of \code{w}, used only when \code{w} is \code{NULL}. The default value is 100.
	  \item \code{batch_len}: Integer; the batch length for the algorithm. The default value is 50.
	  \item \code{N}: Integer; the number of multivariate \eqn{t} samples to use in the simulation.
	  \item \code{conf.level}: Numeric; the desired confidence level.
	  \item \code{buffer}: Numeric; the proportion of either side of the range of data to ignore when defining \code{w}, used only when \code{w} is \code{NULL}.
	  \item \code{curve_args}: An optional list of arguments passed to \code{\link{lines}} (not \code{\link{curve}}!), used to create the boundaries of the confidence band.
	  \item \code{polygon_args}: An optional list of arguments passed to \code{\link{polygon}}, used to shade the confidence region. Setting equal to \code{FALSE} prevents the call, and no shading is done.
	  \item \code{shade_col}: This provides a shortcut to the \code{col} argument of \code{\link{polygon}} to change the color of the shaded region.
	}
	Setting equal to \code{FALSE} prevents calculation and plotting of the confidence band.
}
\item{abline_args}{
	An optional list of arguments to pass to \code{\link{abline}}, used to draw a vertical line at the value of the observed statistic. Setting equal to \code{FALSE} prevents the call, and no line is drawn.
}
  \item{shade_col}{
	Character string or other value specifying the color to use to shade the upper tail of the distribution. Default value is red with 50\% transparency. This is a convenience argument, and the same functionality is available by defining a \code{col} element in the \code{polygon_args} argument.
}
  \item{text_args}{
	An optional list of arguments to pass to \code{\link{text}}, used to label the vertical line with the p-value of the observed statistic. Setting equal to \code{FALSE} prevents the call, and no text is printed.
}
  \item{\dots}{
	Additional arguments to pass to \code{\link{plot}}.
}
}
\details{
	This function produces one of three specified plots, depending on the value of \code{which}.
	
	When \code{which} is "\code{dens}", a plot of the estimated density of the simulated statistics is produced, including a vertical line at the value of the observed test statistic with the p-value displayed.
	
	The default placement of the p-value text is on the left side of the line indicating the statistic value. Specifically, the default values of \code{x} and \code{y} passed to \code{\link{text}} are the statistic value itself and the midpoint between zero and the maximum value of the density curve. The default value passed to \code{adj} is \code{c(1,0.5)}, meaning that the text is aligned to the left of the value \eqn{(x,y)} and centered vertically on it. (The default value for the text itself, which can be modified via the \code{label} argument of \code{\link{text}}, includes a space on the left and the right for padding so the text does not overlap the vertical line itself.) To align the text so it appears on the right side (for example, to avoid overlapping the density curve), use \code{text_args = list(adj = c(0,0.5))}. See documentation for \code{\link{text}} for details on this and other arguments.

  When \code{which} is "\code{residuals}", a time-series-like plot is produced showing transformed residuals in the order given by \code{x$res_order}. In the case that the null hypothesis is rejected, this plot can help determine where (in terms of the linearly ordered covariates) a discrepancy between the model and the data occurs.

  When \code{which} is "\code{epsp}", a plot of the empirical partial sum process is produced; that is, the \eqn{y}-values are
  \deqn{y_j = {1\over\sqrt{n}}\sum_{i=1}^j\hat e_i} where \eqn{\hat e_i} is the \eqn{i}th transformed residual in the order given by \code{x$res_order}. Similar to the case when \code{which} is "\code{residuals}", this plot can help determine where (in terms of the linearly ordered covariates) a discrepancy between the model and the data occurs.
}
\value{
	When \code{which} is "\code{dens}", the values used to create the density plot are returned invisibly in a list with two named elements, \code{x} and \code{y}. If the confidence band is plotted, then it is included as an element named \code{confband}.
	
	For other values of \code{which}, nothing is returned.
}
\author{
	Jesse Miller
}
\references{
Flegal, James M. et al. \strong{Simultaneous confidence bands for (Markov chain) Monte Carlo simulations}, forthcoming.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{distfreereg}}
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
