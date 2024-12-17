\name{qlr.cv}
\alias{qlr.cv}
\title{Calculates the critical value in a Quandt Likelihood Ratio-Test for Structural Breaks in a Parameter with Unknown Break Date}
\usage{
qlr.cv(tAll, from = round(0.15*tAll), to = round(0.85*tAll),
L = 2, sig.level = list(0.05, 0.01, 0.1))
}
\arguments{
\item{tAll}{sample size.}
\item{from}{start period of range to be analyzed for a break.}
\item{to}{end period of range to be analyzed for a break.}
\item{L}{number of parameters.}
\item{sig.level}{significance level. Allowed values are 0.01, 0.05 or 0.10.}
}
\value{
A list object including:
\tabular{ll}{
\code{lambda} \tab	the lambda correction value for the critical value.\cr
\code{range} \tab range of values.\cr
\code{cv.chi2} \tab	critical value of chi^2-test statistics.\cr
\code{cv.f} \tab critical value of F-test statistics.\cr
}
}

\description{
Calculates critical values for Quandt Likelihood Ratio-test (QLR) for structural breaks with unknown break date.
}
\examples{
qlr.cv(20, L = 2, sig.level = 0.01)

}
\references{
Quandt, R.E. (1960): Tests of the Hypothesis That a Linear Regression Obeys Two Separate Regimes. Journal of the American Statistical Association 55, 324–30.

Hansen, B. (1996): “Inference When a Nuisance Parameter is Not Identified under the Null Hypothesis,” Econometrica,
64, 413–430.
}
\concept{Quandt Likelihod Ratio test}
\concept{structural break}
\concept{unknown breakdate}
