\name{B2Nsir}
\alias{B2Nsir}
\title{
Bayesian analysis of the means of two Normal samples using SIR priors.
}
\description{
Produces exploratory plots (boxplots and, if the sample sizes are equal),
a quantile-quantile plot of the two samples. Also produces Bayesian
posterior densities of the two sample means and of the difference between
the means. The priors used are standard improper reference priors.
}
\usage{
B2Nsir(formula, data, var.equal = TRUE, alpha = 0.05, plotit = TRUE, r = 10000)
}
\arguments{
  \item{formula}{
the standard formula interface: response ~ factor
}
  \item{data}{
a data.frame containing the response and the two-level factor
}
  \item{var.equal}{
if TRUE the group variances are assumed to be equal, if FALSE two
separate group variances are estimated
}
  \item{alpha}{
1 - level of credibility, so that for alpha = 0.05 (the default)
credible intervals will have 95\% credibility
}
  \item{plotit}{
should plots be produced?
}
  \item{r}{
the number of samples from the posterior distribution; can usually be
left at its default value of 10000
}
}
\details{
  Note that in the first plot the second sub-plot is NOT a normality
  plot but a quantile-quantile plot that compares the observations in
  the two groups.
  }
\value{
none returned; the function produces several plots and prints some statistics.
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}

\seealso{
  \code{\link{BnNsir}}
  
  \code{\link{t.test}}
}
\examples{
data(bodytemp)
B2Nsir(temperature ~ gender, bodytemp)
}

\keyword{ models }
\keyword{ htest }
