\name{dw.test}
\alias{dw.test}
\title{Durbin-Watson Test on AR(1) Autocorrelation}
\usage{
dw.test(
  mod,
  data = list(),
  dir = c("left", "right", "both"),
  method = c("pan1", "pan2", "paol", "spa"),
  crit.val = TRUE,
  sig.level = 0.05,
  details = FALSE,
  hyp = TRUE
)
}
\arguments{
\item{mod}{estimated linear model object or formula describing the model.}

\item{data}{if \code{mod} is a formula then the corresponding data frame has to be specified.}

\item{dir}{direction of the alternative hypothesis: \code{"right"} for rho > 0, \code{"left"} for rho < 0 and \code{"both"} for rho <> 0.}

\item{method}{algorithm used to calculate the p-value. \code{"pan1"} and \code{"pan2"} are two implementations of Imhof's (1961) algorithm. If they provide a p-values, it is the exact one. \code{"paol"} is Paoella's (2007) re-implementation of Imhof's theory, \code{"spa"} is a saddle point approximation, also implemented by Paoella (2007).}

\item{crit.val}{logical value indicating whether the critical value should be calculated.}

\item{sig.level}{significance level. Default value: \code{sig.level = 0.05}.}

\item{details}{logical value indicating whether specific details about the test should be returned.}

\item{hyp}{logical value indicating whether the Hypotheses should be returned.}
}
\value{
A list object including:
\tabular{ll}{
\code{hyp} \tab character matrix of hypotheses (if \code{hyp = TRUE}).\cr
\code{results} \tab a data frame of basic test results, including critical- and p-value.\cr
\code{nulldist} \tab type of the null distribution (for internal use).\cr
}
}
\description{
Durbin-Watson Test on AR(1) autocorrelation of errors in a linear model. The object of test results returned by this command can be plotted using the \code{plot()} function.
}
\examples{
## Estimate a simple model
filter.est <- ols(sales ~ price, data = data.filter)

## Perform Durbin Watson test for positive autocorrelation rho > 0 (i.e. d < 2)
test.results <- dw.test(filter.est)

## Print the test results
test.results

## Calculate DW null-distribution and plot the test results
plot(test.results)

}
\references{
Durbin, J. & Watson, G.S. (1950): Testing for Serial Correlation in Least Squares Regression I. Biometrika 37, 409-428.

Paolella (2007): Intermediate Probability - A Computational Approach, Wiley.
}
\seealso{
\code{\link{ddw}}, \code{\link{pdw}}.
}
\concept{AR1}
\concept{Durbin Watson test}
\concept{autocorrelation}
