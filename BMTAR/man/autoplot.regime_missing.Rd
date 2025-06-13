\name{autoplot.regime_missing}
\alias{autoplot.regime_missing}

\title{regime_missing object ggplot for the outputs on the function outputs mtarmissing}

\description{
Produces a ggplot object for the results of the mtarmissing function.
}
\usage{
\method{autoplot}{regime_missing}(object, type = 1,...)
}

\arguments{
\item{object}{Object of class \dQuote{\code{regim_missing}}. Not NULL}
\item{type}{character string giving the type of plot to be computed. Allowed
values are 1 for "\code{Missing data (Yt) chains}" (the default) or 2 for \dQuote{\code{Missing data (Ut = [Zt,Xt]) chains}}.
}
\item{...}{other arguments passed to specific methods}
}
\details{
Graph the strings for the outputs corresponding to the functions \dQuote{\code{mtarmissing}} which return an object of class \dQuote{\code{regim_missing}}. The chains corresponding to the samplings in each case do not contain the burning period.
}
\value{
  Return a ggplot object.
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\seealso{\code{\link[BMTAR]{mtarmissing}}}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
library(ggplot2)
data('missingest')
autoplot.regime_missing(missingest,1)
}
