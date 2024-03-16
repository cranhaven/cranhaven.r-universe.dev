\name{t_stat}
\alias{t_stat}
\title{Arima coefficients tests}
\description{It computes the t-statistics tests for the coefficients of an Arima model}
\usage{
t_stat(modarima, decim = 6)
}
\arguments{
 \item{modarima}{an \code{Arima} object}
 \item{decim}{an integer, the precision of the results}
}
\details{\code{modarima} may be created with the function \code{Arima} (package \code{forecast}) or \code{arimax} (package TSA)}
\value{
It returns a matrix   2 x (number of free coefficients)  of the t-statistics and the p-values.
}

\author{Yves Aragon}

\examples{
if(require("forecast"))
{set.seed(123)
y1 = arima.sim(n=100,list(ar=-.7), sd=sqrt(4))
my1 = Arima(y1, order=c(1,0,0),include.mean = FALSE)
t_stat(my1)
}
}
\keyword{ts}
