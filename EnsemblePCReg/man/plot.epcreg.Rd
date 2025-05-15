\name{plot.epcreg}
\alias{plot.epcreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot function for \code{epcreg} model
}
\description{
Function for generating diagnostics plot for \code{epcreg} trained model.
}
\usage{
\method{plot}{epcreg}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Object of class \code{"epcreg"}, typically the output of function \code{\link{epcreg}}.}
  \item{...}{Arguments passed to/from other methods.}
}

\value{
Function \code{plot.epcreg} creates two sub-plots in a figure: 1) a plot of base learner CV errors, with one data point per base learner configuration. The horizontal dotted line indicates the CV error corresponding to the chosen base learner configuration. For "default" method, this is the same as the minimum error of points on this plot; 2) plot of CV error as a function number of PC's used in PCR-based integration. The minimum point of this plot is chosen as the optimal number of PC's and subsequrnyl used for prediction.
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}
