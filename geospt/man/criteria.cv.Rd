\name{criteria.cv}
\alias{criteria.cv}

\title{
Cross-validation summaries
}
\description{
Generate a data frame of statistical values associated with cross-validation
}

\usage{
criteria.cv(m.cv)
}

\arguments{
  \item{m.cv}{
data frame containing: the coordinates of data, prediction columns,
prediction variance of cross-validation data points, observed values,
residuals, zscore (residual divided by kriging standard error), and
fold. If the \code{\link{rbf.tcv}} function is used, the prediction variance
and zscore (residual divided by standard error) will have NA's
}
}
\value{
data frame containing: mean prediction errors (MPE), average kriging
standard error (AKSE), root-mean-square prediction errors (RMSPE),
mean standardized prediction errors (MSPE), root-mean-square standardized
prediction errors (RMSSPE), mean absolute percentage prediction errors (MAPPE), coefficient of correlation of the prediction errors (CCPE), coefficient of determination (R2) and squared coefficient of correlation of the prediction errors (pseudoR2)
}

\examples{
library(gstat)
data(meuse)
coordinates(meuse) <- ~x+y
m <- vgm(.59, "Sph", 874, .04)

# leave-one-out cross validation:
out <- krige.cv(log(zinc)~1, meuse, m, nmax = 40)
criteria.cv(out)

# multiquadratic function
data(preci)
coordinates(preci) <- ~x+y

# predefined eta
tab <- rbf.tcv(prec~x+y,preci,eta=1.488733, rho=0, n.neigh=9, func="M")
criteria.cv(tab)
}

\keyword{ spatial }
