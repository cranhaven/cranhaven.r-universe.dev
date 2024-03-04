\name{plot_fit_dist}
\alias{plot_fit_dist}

\title{
Graphical representation of data fitting to a distribution
}
\description{
A function to help assessing the distribution that best fit a data vector
}
\usage{
plot_fit_dist(x, dist)
}

\arguments{
  \item{x}{
A numeric vector of length at least one containing only finite values (values must be >= 0)
}
  \item{dist}{
 Character vector indicating the distribution to be ploted:"norm",  "lnorm",  "geom", "exp",  "pois", "cauchy",  "logis",  "weibull"
  }
}

\value{
Returns:
   Empirical and theoretical density plots,
   Empirical and theoretical CDFs,
   Q-Q plot,
   P-P plot
}

\author{
F. Barrio-Parra
}



\seealso{
plotdist from Library (fitdstrplus)
}
\examples{

set.seed(123)
a <- rnorm(n = 100, mean = 10, sd = 1)
plot_fit_dist(a, "norm")
}
\keyword{ graphs }
