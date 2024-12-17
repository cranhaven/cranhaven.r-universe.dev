\name{bp.test}
\alias{bp.test}
\title{Breusch-Pagan Test}
\usage{
bp.test(
  mod,
  data = list(),
  varmod = NULL,
  koenker = TRUE,
  sig.level = 0.05,
  details = FALSE,
  hyp = TRUE
)
}
\arguments{
\item{mod}{estimated linear model object or formula.}

\item{data}{if \code{mod} is a formula then the corresponding data frame has to be specified.}

\item{varmod}{formula object (starting with tilde ~) specifying the terms of regressors that explain sigma squared for each observation. If not specified the regular model \code{mod} is used.}

\item{koenker}{logical value specifying whether Koenker's studentized version or the original Breusch-Pagan test should be performed.}

\item{sig.level}{significance level. Default value: \code{sig.level = 0.05}.}

\item{details}{logical value indicating whether specific details about the test should be returned.}

\item{hyp}{logical value indicating whether the Hypotheses should be returned.}
}
\value{
List object including:
\tabular{ll}{
\code{hyp} \tab character matrix of hypotheses (if \code{hyp = TRUE}).\cr
\code{results} \tab a data frame of basic test results.\cr
\code{hreg} \tab matrix of aux. regression results..\cr
\code{stats} \tab additional statistic of aux. regression..\cr
\code{nulldist} \tab type of the Null distribution with its parameters.\cr
}
}
\description{
Breusch-Pagan test for heteroskedastic errors. The object of test results returned by this command can be plotted using the \code{plot()} function.
}
\examples{
## BP test with Koenker's studentized residuals
X <- bp.test(wage ~ educ + age, data = data.wage, koenker = FALSE)
X

## A white test for the same model (auxiliary regression specified by \code{varmod})
bp.test(wage ~ educ + age, varmod = ~ (educ + age)^2 + I(educ^2) + I(age^2), data = data.wage)

## Similar test
wh.test(wage ~ educ + age, data = data.wage)

## Plot the test result
plot(X)

}
\references{
Breusch, T.S. & Pagan, A.R. (1979): A Simple Test for Heteroscedasticity and Random Coefficient Variation. Econometrica 47, 1287-1294.

Koenker, R. (1981): A Note on Studentizing a Test for Heteroscedasticity. Journal of Econometrics 17, 107-112.
}
\seealso{
\code{\link{wh.test}}, \code{\link[lmtest]{bptest}}.
}
\concept{Breuch Pagan test}
\concept{heteroskedasticity}
\concept{non-constant error variance}
