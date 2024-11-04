\name{print.summary.orcutt}
\alias{print.summary.orcutt}
\title{
Summarizing Cochrane-Orcutt Fits
}
\description{
summary method for class "orcutt".
}
\usage{
\method{print}{summary.orcutt}(x, ...)
}
\arguments{
\item{x}{an object of class "orcutt", usually, a result of a call to cochrane.orcutt.}
\item{\dots}{further arguments passed to or from other methods.}
}
\value{
The function summary.orcutt computes and returns a list of summary statistics of the fitted Cochrane-Orcutt.
\item{coefficients}{ a \emph{p x 4} matrix with columns for the estimated coefficient, its standard error, t-statistic and corresponding (two-sided) p-value. Aliased coefficients are omitted. }
\item{fstatistic}{ value of F statistic. }
\item{df}{ degrees of freedom of F statistic. }
\item{r.squared}{ \emph{R}^2, the fraction of variance explained by the model. }
\item{adj.r.squared}{ the above \emph{R}^2 statistic \emph{adjusted}, penalizing for higher \emph{p}. }
\item{DW.t}{ a 4-vector contained the Durbin-Watson statistic and the p-value for the original "lm" model, and the Durbin-Watson statistic and the p-value for the original "orcutt" model . }
}
\references{
Verbeek M. (2004) \emph{A guide to modern econometrics}, John Wiley & Sons Ltd
}
\author{
Stefano Spada
}
\examples{
##-- Continuing the  cochrane.orcutt(.) example:
\donttest{
summary(coch)
}
}
\keyword{ summary.orcutt }