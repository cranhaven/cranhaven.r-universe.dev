\name{cochrane.orcutt}
\alias{cochrane.orcutt}
\title{
Cochrane-Orcutt Estimation
}
\description{
Interactive method using to solve first order autocorrelation problems. This procedure estimates both autocorrelation and beta coefficients recursively until we reach the convergence (8th decimal). The residuals are computed after estimating Beta using EGLS approach and Rho is estimated using the previous residuals
}
\usage{
cochrane.orcutt(reg, convergence = 8, max.iter=100)
}
\arguments{
  \item{reg}{
a linear model built with lm function
}
\item{convergence}{
decimal value to reach for convergence, 8 as default
}
\item{max.iter}{
the maximum number of interactions, 100 as default
}
}
\value{
An object of class "orcutt", basically a list including elements

\item{coefficients}{ a named vector of coefficients. }
\item{residuals}{ residuals. }
\item{fitted.values}{ the fitted mean values.}
\item{t.value}{ t test of coefficients. }
\item{p.value}{ p-value of coefficients. }
\item{call}{ the matched call. }
\item{rho}{ Spearman's rho autocorrelation. }
\item{number.interaction}{ number of interaction of the model. }
\item{DW}{ vector contained Durbin-Watson statistics and p-value. }
}
\references{
Verbeek M. (2004) \emph{A guide to modern econometrics}, John Wiley & Sons Ltd, ISBN:978-88-08-17054-5
}
\author{
Stefano Spada
}
\examples{
\donttest{
data(icecream, package="orcutt")
lm = lm(cons ~ price + income + temp, data=icecream)
coch = cochrane.orcutt(lm)
coch
}
}
\keyword{ lm }
