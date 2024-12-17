\name{wh.test}
\alias{wh.test}
\title{White Heteroskedasticity Test}
\usage{
wh.test(mod, data = list(), sig.level = 0.05, details = FALSE, hyp = TRUE)
}
\arguments{
\item{mod}{estimated linear model object or formula.}

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
\code{hreg} \tab matrix of aux. regression results.\cr
\code{stats} \tab additional statistic of aux. regression.\cr
\code{nulldist} \tab type of the null distribution with its parameters.\cr
}
}
\description{
White's test for heteroskedastic errors.
}
\examples{
## White test for a model with two regressors
X <- wh.test(wage ~ educ + age, data = data.wage)

## Show the auxiliary regression results
X$hreg

## Prettier way
print(X, details = TRUE)

## Plot the test result
plot(X)

}
\references{
White, H. (1980): A Heteroskedasticity-Consistent Covariance Matrix Estimator and a Direct Test for Heteroskedasticity. Econometrica 48, 817-838.
}
\seealso{
\code{\link[lmtest]{bptest}}.
}
\concept{White test}
\concept{heteroscedastic errors}
\concept{heteroskedastic errors}
