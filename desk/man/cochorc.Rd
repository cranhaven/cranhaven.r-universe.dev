\name{cochorc}
\alias{cochorc}
\title{Estimating Linear Models under AR(1) with Cochrane-Orcutt Iteration}
\usage{
cochorc(
  mod,
  data = list(),
  iter = 10,
  tol = 0.0001,
  pwt = TRUE,
  details = FALSE
)
}
\arguments{
\item{mod}{estimated linear model object or formula.}

\item{data}{data frame to be specified if \code{mod} is a formula.}

\item{iter}{maximum number of iterations to be performed.}

\item{tol}{iterations are carried out until difference in rho values is not larger than \code{tol}.}

\item{pwt}{build first observation using Prais-Whinston transformation. If \code{pwt = FALSE} then the first observation is dropped, Default value: \code{pwt = TRUE}.}

\item{details}{logical value, indicating whether details should be printed.}
}
\value{
A list object including:
\tabular{ll}{
\code{results} \tab data frame of iterated regression results.\cr
\code{niter} \tab number of iterated regressions performed.\cr
\code{rho.opt} \tab rho-value at last iteration performed..\cr
\code{y.trans} \tab transformed y-values at last iteration performed.\cr
\code{X.trans} \tab transformed x-values (incl. z) at last iteration performed.\cr
\code{resid} \tab residuals of transformed model estimation.\cr
\code{all.regs} \tab data frame of regression results for all considered rho-values.\cr
}
}
\description{
If autocorrelated errors can be modeled by an AR(1) process (rho as parameter) then this function performs a Cochrane-Orcutt iteration. If model coefficients and the estimated rho value converge with the number of iterations, this procedure provides valid solutions. The object returned by this command can be plotted using the \code{plot()} function.
}
\examples{
## In this example only 2 iterations are needed to achieve (convergence of rho at the 5th digit)
sales.est <- ols(sales ~ price, data = data.filter)
cochorc(sales.est)

## For a higher precision we need 6 iterations
cochorc(sales.est, tol = 0.0000000000001)

## Direct usage of a model formula
X <- cochorc(sick ~ jobless, data = data.sick[1:14,], details = TRUE)

## See iterated regression results
X$all.regs

## Print full details
X

## Suppress details
print(X, details = FALSE)

## Plot rho over iterations to see convergence
plot(X)

## Example with interaction
dummy <-  as.numeric(data.sick$year >= 2005)
kstand.str.est <- ols(sick ~ dummy + jobless + dummy*jobless, data = data.sick)
cochorc(kstand.str.est)

}
\references{
Cochrane, E. & Orcutt, G.H. (1949): Application of Least Squares Regressions to Relationships Containing Autocorrelated Error Terms. Journal of the American Statistical Association 44, 32-61.
}
