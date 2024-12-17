\name{pdw}
\alias{pdw}
\title{Durbin-Watson Distribution}
\usage{
pdw(x, mod, data = list())
}
\arguments{
\item{x}{quantile value(s) at which the density should be determined.}

\item{mod}{estimated linear model object, formula (with \code{data} specified), or model matrix.}

\item{data}{if \code{mod} is a formula then the name of the corresponding data frame has to be specified.}
}
\value{
Numerical density value(s).
}
\description{
Calculates cumulative distribution values of the null distribution in the Durbin-Watson test. Uses saddle point approximation by Paolella (2007).
}
\details{
Distribution depends on values of the exogenous variables. That is why it must be calculated from each specific data set, respectively.
}
\examples{
filter.est <- ols(sales ~ price, data = data.filter)
pdw(x = c(0.9, 1.7, 2.15), filter.est)

}
\references{
Paolella, M.S. (2007): Intermediate Probability - A Computational Approach, Wiley.
}
\seealso{
\code{\link{ddw}}, \code{\link{dw.test}}.
}
\concept{Durbin-Watson}
\concept{null distribution}
