\name{pc.test}
\alias{pc.test}
\title{Prognostic Chow Test on Structural Break}
\usage{
pc.test(
  mod,
  data = list(),
  split,
  sig.level = 0.05,
  details = FALSE,
  hyp = TRUE
)
}
\arguments{
\item{mod}{the regular model (estimated or formula) without dummy variables.}

\item{data}{if \code{mod} is a formula then the corresponding data frame has to be specified.}

\item{split}{number of periods in phase I (last period before suspected break). Phase II is the total of remaining periods.}

\item{sig.level}{significance level. Default value: \code{sig.level = 0.05}.}

\item{details}{logical value indicating whether specific details (null distribution, number of periods, and SSRs) of the test should be displayed.}

\item{hyp}{logical value indicating whether the hypotheses should be displayed.}
}
\value{
A list object including:
\tabular{ll}{
\code{hyp} \tab the null-hypothesis to be tested.\cr
\code{results} \tab data frame of test results.\cr
\code{SSR1} \tab sum of squared residuals of phase I.\cr
\code{SSR} \tab sum of squared residuals of phase I + II.\cr
\code{periods1} \tab	number of periods in Phase I.\cr
\code{periods.total} \tab total number of periods.\cr
\code{nulldist} \tab the null distribution in the test.\cr
}
}
\description{
Performs prognostic Chow test on structural break. The object of test results returned by this command can be plotted using the \code{plot()} function.
}
\examples{
## Estimate model
unemp.est <- ols(unempl ~ gdp, data = data.unempl[1:14,])

## Test for immediate structural break after t = 13
X <- pc.test(unemp.est, split = 13, details = TRUE)
X

plot(X)

}
\references{
Chow, G.C. (1960): Tests of Equality Between Sets of Coefficients in Two Linear Regressions. Econometrica 28, 591-605.
}
\concept{Chow test}
\concept{prognostic Chow test}
\concept{structural break}
