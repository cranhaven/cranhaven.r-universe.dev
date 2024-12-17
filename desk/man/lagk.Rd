\name{lagk}
\alias{lagk}
\title{1 to k-Period Lags of Given Vector}
\usage{
lagk(u, lag = 1, delete = TRUE)
}
\arguments{
\item{u}{a vector of one variable, usually residuals.}

\item{lag}{the number of periods up to which lags should be generated.}

\item{delete}{logical value indicating whether missing data should be eliminated from the resulting matrix.}
}
\value{
Matrix of vector \code{u} and its 1 to k-period lags.
}
\description{
Generates a matrix of a given vector and its 1 to k-period lags. Missing values due to lag are filled with NAs.
}
\examples{
u = round(rnorm(10),2)
lagk(u)
lagk(u,lag = 3)
lagk(u,lag = 3, delete = FALSE)

}
\concept{heteroscedasticity}
\concept{lag}
\concept{lagged residuals}
