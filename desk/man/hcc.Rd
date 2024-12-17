\name{hcc}
\alias{hcc}
\title{Heteroskedasticity Corrected Covariance Matrix}
\usage{
hcc(mod, data = list(), digits = 4)
}
\arguments{
\item{mod}{estimated linear model object or formula.}

\item{data}{if \code{mod} is a formula then the corresponding data frame has to be specified.}

\item{digits}{number of decimal digits in rounded values.}
}
\value{
The heteroskedasticity corrected covariance matrix.
}
\description{
Calculates Whites (1980) heteroskedasticity corrected covariance matrix in a linear model.
}
\examples{
rent.est <- ols(rent ~ dist, data = data.rent)
hcc(rent.est)

hcc(wage ~ educ + age, data = data.wage)

}
\references{
White, H. (1980): A Heteroskedasticity-Consistent Covariance Matrix Estimator and a Direct Test for Heteroskedasticity. Econometrica 48, 817-838.
}
\seealso{
\code{\link{wh.test}}, \code{\link[lmtest]{bptest}}.
}
\concept{correction}
\concept{heteroskedasticity}
