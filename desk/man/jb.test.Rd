\name{jb.test}
\alias{jb.test}
\title{Jarque-Bera Test}
\usage{
jb.test(x, data = list(), sig.level = 0.05, details = FALSE, hyp = TRUE)
}
\arguments{
\item{x}{a numeric vector, an estimated linear model object or model formula (with \code{data} specified). In the two latter cases the model's residuals are tested for normality.}

\item{data}{if \code{mod} is a formula then the corresponding data frame has to be specified.}

\item{sig.level}{significance level. Default value: \code{sig.level = 0.05}.}

\item{details}{logical value indicating whether specific details about the test should be returned.}

\item{hyp}{logical value indicating whether the hypotheses should be returned.}
}
\value{
A list object including:
\tabular{ll}{
\code{hyp} \tab character matrix of hypotheses (if \code{hyp = TRUE}).\cr
\code{results} \tab a data frame of basic test results.\cr
\code{skew} \tab moment of order 3 (asymmetry, skewness).\cr
\code{kur} \tab moment of order 4 (kurtosis).\cr
\code{nobs} \tab number of observations (internal purpose).\cr
\code{nulldist} \tab type of the Null distribution and its parameter(s).\cr
}
}
\description{
Jarque-Bera test for normality. The object of test results returned by this command can be plotted using the \code{plot()} function.
}
\details{
Under H0 the test statistic of the Jarque-Bera test follows a chi-squared distribution with 2 degrees of freedom. If moment of order 3 (skewness) differs significantly from 0 and/or moment of order 4 (kurtosis) differs significantly from 3, H0 is rejected.
}
\examples{
## Test response variable for normality
X <- jb.test(data.income$loginc)
X

## Estimate linear model
income.est <- ols(loginc ~ logsave + logsum, data = data.income)
## Test residuals for normality, print details
jb.test(income.est, details = TRUE)

## Equivalent test
jb.test(loginc ~ logsave + logsum, data = data.income, details = TRUE)

## Plot the test result
plot(X)

}
\references{
Jarque, C.M. & Bera, A.K. (1980): Efficient Test for Normality, Homoscedasticity and Serial Independence of Residuals. Economics Letters 6 Issue 3, 255-259.
}
\seealso{
`jarque.test()` in Package `moments`.
}
\concept{normal distribution}
\concept{normality}
\concept{test}
