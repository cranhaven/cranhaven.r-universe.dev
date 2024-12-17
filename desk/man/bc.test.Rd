\name{bc.test}
\alias{bc.test}
\title{Box-Cox Test}
\usage{
bc.test(
  basemod,
  data = list(),
  exo = "same",
  sig.level = 0.05,
  details = TRUE,
  hyp = TRUE
)
}
\arguments{
\item{basemod}{estimated linear model object or formula taken as the base model for comparison. Has to have a non-transformed endogenous variable.}

\item{data}{if \code{mod} is a formula then the corresponding data frame has to be specified.}

\item{exo}{vector or matrix of transformed exogenous variables to be used in the comparison model. If not specified the same variables from the base model are used ("same").}

\item{sig.level}{significance level. Default value: \code{sig.level = 0.05}.}

\item{details}{logical value indicating whether specific details about the test should be returned.}

\item{hyp}{logical value indicating whether the Hypotheses should be returned.}
}
\value{
A list object including:
\tabular{ll}{
\code{hyp} \tab character matrix of hypotheses (if \code{hyp = TRUE}).\cr
\code{results} \tab a data frame of basic test results.\cr
\code{stats} \tab additional statistic of aux. regression.\cr
\code{nulldist} \tab type of the Null distribution with its parameters.\cr
}
}
\description{
Box-Cox test for functional form. Compares a base model with non transformed endogenous variable to a model with logarithmic endogenous variable. Exogenous variables can be transformed or non-transformed. The object of test results returned by this command can be plotted using the \code{plot()} function.
}
\examples{
## Box-Cox test between a semi-logarithmic model and a logarithmic model
semilogmilk.est <- ols(milk ~ log(feed), data = data.milk)
results <- bc.test(semilogmilk.est, details = TRUE)

## Plot the test results
plot(results)

## Example with transformed exogenous variables
lin.est <- ols(rent ~ mult + mem + access, data = data.comp)
A <- lin.est$data
bc.test(lin.est, exo = log(cbind(A$mult, A$mem, A$access)))

}
\references{
Box, G.E.P. & Cox, D.R. (1964): An Analysis of Transformations. Journal of the Royal Statistical Society, Series B. 26, 211-243.
}
\seealso{
\code{\link[MASS]{boxcox}}.
}
\concept{htest}
