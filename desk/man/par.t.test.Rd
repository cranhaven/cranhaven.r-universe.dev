\name{par.t.test}
\alias{par.t.test}
\title{t-Test on Estimated Parameters of a Linear Model}
\usage{
par.t.test(
  mod,
  data = list(),
  nh,
  q = 0,
  dir = c("both", "left", "right"),
  sig.level = 0.05,
  details = FALSE,
  hyp = TRUE
)
}
\arguments{
\item{mod}{model object estimated by \code{ols()} or \code{lm()}.}

\item{data}{name of the data frame to be used if \code{mod} is a formula and the variables are not present in the environment.}

\item{nh}{vector of the coefficients of the linear combination of parameters.}

\item{q}{value on which parameter (combination) is to be tested against. Default value: q = 0.}

\item{dir}{direction of the hypothesis: \code{"both"}, \code{"left"}, \code{"right"}, Default value: \code{"both"}.}

\item{sig.level}{significance level. Default value: \code{sig.level = 0.05}.}

\item{details}{logical value indicating whether specific details about the test should be returned.}

\item{hyp}{logical value indicating whether the Hypotheses should be returned.}
}
\value{
A list object including:
\tabular{ll}{
\code{hyp} \tab character matrix of hypotheses (if \code{hyp = TRUE}).\cr
\code{nh} \tab null hypothesis as parameters of a linear combination (for internal purposes).\cr
\code{lcomb} \tab the linear combination of parameters tested.\cr
\code{results} \tab a data frame of basic test results.\cr
\code{std.err} \tab standard error of the linear estimator.\cr
\code{nulldist} \tab type of the null distribution with its parameters.\cr
}
}
\description{
Performs a t-test on a single parameter hypothesis or a hypothesis containing a linear combination of parameters of a linear model. The object of test results returned by this command can be plotted using the \code{plot()} function.
}
\examples{
## Test H1: "phos + nit <> 1"
fert.est <- ols(barley ~ phos + nit, data = log(data.fertilizer))
x = par.t.test(fert.est, nh = c(0,1,1), q = 1, details = TRUE)
x # Show the test results

plot(x) # Visualize the test result

## Test H1: "phos > 0.5"
x = par.t.test(fert.est, nh = c(0,1,0), q = 0.5, dir = "right")
plot(x)

}
\concept{linear hypothesis}
\concept{linear model}
\concept{t-test}
