\name{makedata.bc}
\alias{makedata.bc}
\title{Generate Artificial, Non-linear Data for Simple Regression}
\usage{
makedata.bc(
  lambda.x = 1,
  lambda.y = 1,
  a = 0,
  x.max = 5,
  n = 200,
  sigma = 1,
  seed = NULL
)
}
\arguments{
\item{lambda.x}{deformation parameter for the x-values: -1 = inverse, 0 = log, 0.5 = root, 1 = linear, 2 = square ...}

\item{lambda.y}{deformation parameter for the y-values (see \code{lambda.x}).}

\item{a}{additive constant to shift the data in vertical direction.}

\item{x.max}{upper border of x values, must be greater than 1.}

\item{n}{number of artificial observations.}

\item{sigma}{standard deviation of the error term.}

\item{seed}{randomization seed.}
}
\value{
Data frame of x- and y-values.
}
\description{
This command generates a data frame of two variables, x and y, which can be both transformed by a normalized, lambda-deformed logarithm (aka. Box-Cox-transformation). The purpose of this command is to generate data sets that represent a non-linear relationship between exogenous and endogenous variable. These data sets can be used to train linearization and heteroskedasticity issues. Note that the error term is also transformed to make it normal and homoscedastic after re-transformation to linearity. This is why generated data sets may have non-constant variance depending on the transformation parameters.
}
\examples{
## Compare 4 data sets generated differently
parOrg = par("mfrow")
par(mfrow = c(2,2))

## Linear data shifted by 3
A.dat <- makedata.bc(a = 3)

## Log transformed y-data
B.dat <- makedata.bc(lambda.y = 0, n = 100, sigma = 0.2, x.max = 2, seed = 123)

## Concave scatter
C.dat <- makedata.bc(lambda.y = 6, sigma = 0.4, seed = 12)

## Concave scatter, x transf.
D.dat <- makedata.bc(lambda.x = 0, lambda.y = 6, sigma = 0.4, seed = 12)

plot(A.dat, main = "linear data shifted by 3")
plot(B.dat, main = "log transformed y-data")
plot(C.dat, main = "concave scatter")
plot(D.dat, main = "concave scatter, x transf.")
par(mfrow = parOrg)

}
\concept{confidence band}
\concept{plot}
\concept{prediction band}
\concept{regression line}
\concept{scatter}
