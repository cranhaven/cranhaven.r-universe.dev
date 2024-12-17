\name{qlr.test}
\alias{qlr.test}
\title{Quandt Likelihood Ratio-Test for Structural Breaks in any Parameter with Unknown Break Date}
\usage{
qlr.test(mod, data = list(), from, to, sig.level = 0.05, details = FALSE)
}
\arguments{
\item{mod}{the regular model object (without dummies) estimated by \code{ols()} or \code{lm()}.}

\item{data}{name of the data frame to be used if \code{mod} is only a formula.}

\item{from}{start period of range to be analyzed for a break.}

\item{to}{end period of range to be analyzed for a break.}

\item{sig.level}{significance level. Allowed values are 0.01, 0.05 or 0.10.}

\item{details}{logical value indicating whether specific details about the test should be returned.}
}
\value{
A list object including:
\tabular{ll}{
\code{hyp} \tab the null-hypothesis to be tested.\cr
\code{results} \tab data frame of test results.\cr
\code{chi2.stats} \tab	chi^2-test statistics calculated between from and to.\cr
\code{f.stats} \tab F-test statistics calculated between from and to.\cr
\code{f.crit} \tab lower and upper critical F-value.\cr
\code{p.value} \tab p-value in the test using approximation method proposed by Hansen (1997).\cr
\code{breakpoint} \tab period at which largest F-value occurs.\cr
\code{periods} \tab the range of periods analyzed.\cr
\code{lf.crit} \tab lower and upper critical F-value including corresponding lambda values.\cr
\code{lambda} \tab	the lambda correction value for the critical value.\cr
}
}
\description{
Performs Quandt Likelihood Ratio-test (QLR) for structural breaks with unknown break date. The object returned by this command can be plotted using the \code{plot()} function.
}
\examples{
unemp.est <- ols(unempl ~ gdp, data = data.unempl)
my.qlr <- qlr.test(unemp.est, from = 13, to = 17, details = TRUE)
my.qlr # Print test results

plot(my.qlr) # Plot test results

}
\references{
Quandt, R.E. (1960): Tests of the Hypothesis That a Linear Regression Obeys Two Separate Regimes. Journal of the American Statistical Association 55, 324–30.
}
\concept{Quandt Likelihod Ratio test}
\concept{structural break}
\concept{unknown breakdate}
