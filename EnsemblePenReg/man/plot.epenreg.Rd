\name{plot.epenreg}
\alias{plot.epenreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot function for \code{epenreg} model
}
\description{
Function for generating diagnostics plot for \code{epenreg} trained model.
}
\usage{
\method{plot}{epenreg}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Object of class \code{"epenreg"}, typically the output of function \code{\link{epenreg}}.}
  \item{...}{Arguments passed to/from other methods.}
}

\value{
Function \code{plot.epenreg} creates two sub-plots in a figure: 1) a plot of base learner CV errors, with one data point per base learner configuration. The horizontal dotted line indicates the CV error corresponding to the chosen base learner configuration. For "default" method, this is the same as the minimum error of points on this plot; 2) plot of CV error as a function of the value of shrinkage parameter (x-axis in log scale). The minimum point of this plot is chosen as the optimal lambda and subsequently used for prediction.
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}
