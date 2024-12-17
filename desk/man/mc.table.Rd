\name{mc.table}
\alias{mc.table}
\title{Generate R² Matrix of all Possible Regressions Among Regressors to Check Multicollinearity}
\usage{
mc.table(x, intercept = TRUE, digits = 3)
}
\arguments{
\item{x}{data frame of variables to be regressed on each other.}

\item{intercept}{logical value specifying whether regression should have an intercept.}

\item{digits}{number of digits to be rounded to.}
}
\value{
Matrix of R-squared values. The column headers indicate the respective endogenous variables that is projected on a combination of exogenous variables. Example: If we have 4 regressors x1, x2, x3, x4, then the fist column of the returned matrix has 7 rows including the R-squared values of the following regressions:
\enumerate{
  \item x1 ~ x2 + x3 + x4
  \item x1 ~ x3 + x4
  \item x1 ~ x2 + x4
  \item x1 ~ x2 + x3
  \item x1 ~ x4
  \item x1 ~ x3
  \item x1 ~ x2
}
The second column corresponds to the regressions:
\enumerate{
  \item x2 ~ x1 + x3 + x4
  \item x2 ~ x3 + x4
  \item x2 ~ x1 + x4
  \item x2 ~ x1 + x3
  \item x2 ~ x4
  \item x2 ~ x3
  \item x2 ~ x1
}
and so on.
}
\description{
For a given set of regressors this command calculates the coefficient of determination of a regression of one specific regressor on all combinations of the remaining regressors. This provides an overview of potential multicollinearity. Needs at least three variables. For just two regressors the square of \code{cor()} can be used.
}
\examples{
## Replicate table 21.3 in the textbook
mc.table(data.printer[,-1])

}
\concept{coefficient of determination}
\concept{multicollinearity}
