\name{BnNsir}
\alias{BnNsir}
\title{
Bayesian analysis of n >= 2 Normal means with standard improper reference priors.
}
\description{
Several exploratory plots are produced, after which this function
calculates and plots the posterior densities of the treatment means and
their differences. Pooled or separate variances can be specified. Note
that this function uses Standard Improper Reference (SIR) priors. 
}
\usage{
BnNsir(formula, data, var.equal = TRUE, alpha = 0.05, plotit = TRUE,
 r = 10000)
}
\arguments{
  \item{formula}{
the usual formula interface: response ~ factor
}
  \item{data}{
a data.frame containing the response and the factor variables
}
  \item{var.equal}{
should a pooled variance be used? Specify var.equal = FALSE if you want
separate variances to be fitted
}
  \item{alpha}{
1 - level of credibility, so that for alpha = 0.05 (the default)
credible intervals will have 95\% credibility
}
  \item{plotit}{
are plots desired?
}
  \item{r}{
the number of samples of the posterior that should be taken
}
}
\value{
none returned: the function is used for the plots and the printed
information it produces
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{ 
  \code{\link{B2Nsir}} 
}
\examples{
data(PlantGrowth)
BnNsir(weight ~ group, PlantGrowth)
}

\keyword{ models }
\keyword{ htest }
