\name{criteriaST.cv}
\alias{criteriaST.cv}

\title{
Cross-validation summaries
}
\description{
Generate a data frame of statistical values associated with cross-validation
}

\usage{
criteriaST.cv(m.cv)
}

\arguments{
  \item{m.cv}{
data frame containing: prediction columns, prediction variance of cross-validation
data points, observed values, residuals, zscore (residual divided by kriging
standard error), and fold. If the \code{\link{rbfST.tcv}} function is used, the
prediction variance, zscore (residual divided by standard error) will have NA's,
coordinates data and time.
}
}
\value{
data frame containing: mean prediction errors (MPE), average kriging
standard error (AKSE), root-mean-square prediction errors (RMSPE),
mean standardized prediction errors (MSPE), root-mean-square standardized
prediction errors (RMSSPE), mean absolute percentage prediction errors (MAPPE), coefficient of correlation of the prediction errors (CCPE), coefficient of determination (R2) and squared coefficient of correlation of the prediction errors (pseudoR2)
}

\examples{
# leave-one-out cross validation:
data(croatiadb)
coordinates(croatiadb) <- ~x+y

# inverse multiquadratic function, predefined eta and rho
tempm <- rbfST.tcv(MTEMP~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, croatiadb, eta=0.0108,
                   rho=0.00004, n.neigh=25, func="IM")
criteriaST.cv(tempm)
}

\keyword{ spatial }
