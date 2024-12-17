\name{ddw}
\alias{ddw}
\title{Durbin Watson Distribution}
\usage{
ddw(x, mod, data = list())
}
\arguments{
\item{x}{quantile value(s) at which the density should be determined.}

\item{mod}{estimated linear model object, formula (with argument \code{data} specified), or model matrix.}

\item{data}{if \code{mod} is a formula then the name of the corresponding dataframe has to be specified here.}
}
\value{
Numerical density value(s).
}
\description{
Calculates density values of the null distribution in the Durbin Watson test. Uses the saddlepoint approximation by Paolella (2007).
}
\details{
The Durbin Watson Null-Distribution depends on values of the exogenous variables. That is why it must be calculated from each specific data set, respectively.
}
\examples{
filter.est <- ols(sales ~ price, data = data.filter)
ddw(x = c(0.9, 1.7, 2.15), filter.est)

}
\references{
Durbin, J. & Watson, G.S. (1950): Testing for Serial Correlation in Least Squares Regression I. Biometrika 37, 409-428.

Paolella (2007): Intermediate Probability - A Computational Approach, Wiley.
}
\seealso{
\code{\link{dw.test}}, \code{\link{pdw}}.
}
\concept{Durbin Watson}
\concept{null distribution}
