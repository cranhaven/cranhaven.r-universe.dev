\name{scatterplot2.JGR}
\alias{scatterplot2.JGR}
\title{Draws a scatterplot of bivariate data}
\description{
	Creates a scatterplot - a plot displaying the relationship between two
	variables. A linear regression line or loess smooth can be overlayed.
}

\details{
	A scatterplot provides a graphical means of determining the relationship
	between two variables. Important relationships that can be observed are
	linear, higher-order polynomial, exponential, or logarithmic. Along with
	the general pattern, it is possible to qualitatively determine the strength
	of the relationship. The "closer" or "tighter" the points are to the
	general shape, the stronger the relationship. A scatterplot is also a
	useful tool for identifying potential univariate or bivariate outliers
	(observations that are much larger or smaller than the other data points).
	
	After gaining insight into the general relationship of the variables, a
	quantitative analysis method may be more easily chosen:
	
	\itemize{
		\item Correlation analysis, which helps determine the strength of the
				linear relationship between two variables.
		\item Linear regression, which fits a linear model to two or more
				variables.
		\item Quantile regression, which fits a model to conditional quantiles
				of the dependent variable rather than to the conditional mean.
	}

	\bold{User Interface}

	To create a scatterplot, select	"Scatterplot" from the "Analysis Tools"
	menu. A dialog box will appear that allows the user to enter plotting
	parameters. Select the data set of interest from the pull-down
	menu, or browse for a tab-delimited text file.  The Data
	Subsetting tab can be used to select a subset of the data file
	by choosing a variable from the pull down menu and then
	selecting the levels of that variable to include.  You can hold
	down the <CTRL> key to select multiple levels.  A separate plot
	will be produced for each unique setting of the subsetting
	variables. Select a variable to plot on the x-axis and a
	different variable to plot on the y-axis.   A regression fit can
	be overlaid on the scatterplot.  If Linear Regression Line is
	selected, a straight line fit is drawn.  If Loess Smooth is
	selected, a nonparametric loess fit is drawn. You may change the
	axes labels and plot title by typing them in the Labels dialog.  
The output is simply the scatterplot graph with any overlaid fits 
requested. 
	
}

\seealso{
	To learn about boxplots, see \link{boxplot}.

	To learn about correlation analysis, see \link{cor}.
	
	To learn about linear regression, see \link{lm}.
	
	To view the help file for the general R plotting function, see
	\code{\link{plot}}.
}

\keyword{hplot} 

