\name{reset.test}
\alias{reset.test}
\title{RESET Method for Non-linear Functional Form}
\usage{
reset.test(
  mod,
  data = list(),
  m = 2,
  sig.level = 0.05,
  details = FALSE,
  hyp = TRUE
)
}
\arguments{
\item{mod}{estimated linear model object or formula.}

\item{data}{if \code{mod} is a formula then the corresponding data frame has to be specified.}

\item{m}{the number of non-linear terms of fitted y values that should be included in the extended model. Default is \code{m = 2}, i.e. to add \eqn{\widehat{y}^2} and \eqn{\widehat{y}^3}.}

\item{sig.level}{significance level. Default value: \code{sig.level = 0.05}.}

\item{details}{logical value indicating whether specific details about the test should be returned.}

\item{hyp}{logical value indicating whether the Hypotheses should be returned.}
}
\value{
A list object including:
\tabular{ll}{
\code{hyp} \tab character matrix of hypotheses (if \code{hyp = TRUE}).\cr
\code{results} \tab a data frame of basic test results.\cr
\code{SSR0} \tab	SSR of the H0-model.\cr
\code{SSR1} \tab	SSR of the extended model.\cr
\code{L} \tab numbers of parameters tested in H0.\cr
\code{nulldist} \tab	null distribution of the test.\cr
}
}
\description{
Ramsey's RESET for non-linear functional form. The object of test results returned by this command can be plotted using the \code{plot()} function.
}
\examples{
## Numerical illustration 14.2. of the textbook
X <- reset.test(milk ~ feed, m = 4, data = data.milk)
X

## Plot the test result
plot(X)

}
\references{
Ramsey, J.B. (1969): Tests for Specification Error in Classical Linear Least Squares Regression Analysis. Journal of the Royal Statistical Society, Series B 31, 350-371.
}
\seealso{
\code{\link[lmtest]{resettest}}.
}
\concept{RESET}
\concept{functional form}
\concept{specification}
