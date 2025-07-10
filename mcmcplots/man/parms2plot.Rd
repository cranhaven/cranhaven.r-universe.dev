\name{parms2plot}
\alias{parms2plot}

\title{ Matches groups of parameters to plot in MCMC diagnostics plots.}

\description{
  Utility function that finds the parameter names to plot in the \code{mcmcplot} function.  Intended for internal use only.
}

\usage{
parms2plot(parnames, parms, regex, random, leaf.marker = "[\\\\[_]", do.unlist = TRUE)
}

\arguments{
  \item{parnames}{ parameter names from an MCMC run }
  \item{parms}{ partial parameter names that will be used to determine which subset of \code{parnames} will be plotted.}
  \item{regex}{ a vector of character strings containing regular expressions to match parameter names in the \code{mcmc} object.}
  \item{random}{ an integer or \code{NULL}.  If an integer is specified, the function will select only \code{random} number of plots from each parameter group for plotting.  If \code{NULL}, all parameter names from groups specified in \code{params} will be plotted.}
  \item{leaf.marker}{ a regular expression with a character class that marks the beginning of the ``leaf'' portion of a parameter name.  The default character class includes \code{[} and \code{_}}
  \item{do.unlist}{ a logical indicating whether the function should return the vector of parameter names or a list of parameter names according to parameter "groupings" (so parameters can be accessed according to their "stems").  This option was added in order to improve the functionality of \code{mcmcplot} function.}
}

\details{
  The function \code{parms2plot} is used internally by most plotting functions in the \pkg{mcmcplots} package.  The function's purpose is to allow users to conveniently specify groups of parameters to be used in plots of MCMC output.

  \code{parms2plot} relies on using regular expressions to find ``stems'' and ``leaves'' in parameter names and to create groups of parameters.  For example, the parameter \code{beta[10]} has stem \code{beta} and leaf \code{[10]}, and this naming convention indicates that the parameter \code{beta[10]} is part of a larger collection of \code{beta} parameters.
  
  \code{parms2plot} uses a ``leaf marker'' specified by the \code{leaf.marker} argument to determine the end of the parameter stem and the beginning of its leaf.  The default leaf marker is an open left bracket ``['' or an ``_'' as specified by a regular expression character class.
  
  Creating plots of specific groupings of parameters is possible by specifying parameter stems in the \code{parms} argument.  For example, calling the function \code{traplot(mcmcout, parms="beta")} will create a single plot window of trace plots for parameters \code{beta[1]}, \ldots, \code{beta[10]}.
  
  At first glance the leaf-marker concept might seem like overkill.  For example, to plot parameters \code{mu[1]}, \ldots, \code{mu[10]} why not simply use a string matching function to match ``mu'' in the parameter names?  The answer is that other parameter names might also match ``mu'' but may not be part of the grouping \code{mu[1]}, \ldots, \code{mu[10]}.  A model with parameter name \code{mu.gamma} would match the string ``mu'' but is not part of the parameter grouping \code{mu[1]}, \ldots, \code{mu[10]}.  \code{parms2plot} avoids this ``greedy'' matching by requiring an explicit declaration of a leaf marker.
  
  \code{parms2plot} also allows the user to specify regular expressions for more direct control over the groups of parameters that are plotted.  Regular expressions are specified via the \code{regex} argument.  When \code{parms}, \code{regex}, and \code{random} are \code{NULL}, parms2plot will return all parameter names.
    
  The \code{random} option is useful when an MCMC simulation contains a large number of parameters in a group, e.g. in a hierarchical model with one or more parameter per observation in the data set.  In such settings, it is not feasible to create or examine plots for all parameters in a model. The \code{random} argument allows the user to specify a maximum number of plots to create for each parameter grouping.  If a parameter grouping exceeds the number specified in \code{random}, then a number of parameters (as specified in \code{random})  will be randomly selected for plotting.  If \code{random} is a vector, then each element of \code{random} corresponds to a parameter grouping specified in \code{parms} and \code{regex}.  If specified, the \code{random} argument is recycled to be the same length as \code{length(parms) + length(regex)}.  Values of \code{NA} in \code{random} denote parameter groupings where all parameters in the group will be plotted.

}

\value{
  A character vector with parameter names.
}

\author{ S. McKay Curtis with contributions from Ilya Goldin }

\seealso{
  \code{\link{mcmcplot}}, \code{\link{caterplot}}, \code{\link{traplot}}, \code{\link{denplot}}
}

\examples{
prm <- c(paste("gamma[", 1:30, "]", sep=""), paste("alpha[", 1:20, "]", sep=""))

parms2plot(prm, NULL, NULL, NULL)      # returns all
parms2plot(prm, NULL, NULL, 5)         # returns 5 randomly from each group
parms2plot(prm, NULL, NULL, c(5, 10))  # 5 from gamma, 10 from alpha
parms2plot(prm, NULL, NULL, c(10, NA)) # 10 from gamma, all from alpha
parms2plot(prm, "alpha", NULL, NULL)   # all alphas
parms2plot(prm, "gamma", NULL, NULL)   # all gammas
parms2plot(prm, NULL, "alpha\\\\[1[[:digit:]]\\\\]$", NULL)   # alpha[10]-alpha[19]
parms2plot(prm, "gamma", "alpha\\\\[1[[:digit:]]\\\\]$", NULL)  # all gamma and alpha[10]-alpha[19]
}

\keyword{utilities}
