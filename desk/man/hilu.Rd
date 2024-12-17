\name{hilu}
\alias{hilu}
\title{Estimating Linear Models under AR(1) Autocorrelation with Hildreth and Lu Method}
\usage{
hilu(mod, data = list(), range = seq(-1, 1, 0.01), details = FALSE)
}
\arguments{
\item{mod}{estimated linear model object or formula.}

\item{data}{data frame to be specified if \code{mod} is a formula.}

\item{range}{defines the range and step size of rho values.}

\item{details}{logical value, indicating whether details should be printed.}
}
\value{
A list object including:
\tabular{ll}{
\code{results} \tab data frame of basic regression results.\cr
\code{idx.opt} \tab index of regression that minimizes SSR.\cr
\code{nregs} \tab number of regressions performed.\cr
\code{rho.opt} \tab rho-value of regression that minimizes SSR.\cr
\code{y.trans} \tab optimal transformed y-values.\cr
\code{X.trans} \tab optimal transformed x-values (incl. z).\cr
\code{all.regs} \tab data frame of regression results for all considered rho values.\cr
\code{rho.vals} \tab vector of used rho values.\cr
}
}
\description{
If autocorrelated errors can be modeled by an AR(1) process (rho as parameter) then this function finds the rho value that that minimizes SSR in a Prais-Winsten transformed linear model. This is known as Hildreth and Lu estimation. The object returned by this command can be plotted using the \code{plot()} function.
}
\examples{
sales.est <- ols(sales ~ price, data = data.filter)

## In this example regressions over 199 rho values between -1 and 1 are carried out
## The one with minimal SSR is printed out
hilu(sales.est)

## Direct usage of a model formula
X <- hilu(sick ~ jobless, data = data.sick[1:14,], details = TRUE)

## Print full details
X

## Suppress details
print(X, details = FALSE)

## Plot SSR over rho-values to see minimum
plot(X)

}
\references{
Hildreth, C. & Lu, J.Y. (1960): Demand Relations with Autocorrelated Disturbances. AES Technical Bulletin 276, Michigan State University.
}
\concept{AR1 process}
\concept{Hildreth-Lu}
\concept{VKQ}
\concept{autocorrelation}
