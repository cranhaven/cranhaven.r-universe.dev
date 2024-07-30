\name{boot.sd1}
\alias{boot.sd1}
\title{Bootstrap standard error 1
}
\description{Calculates the bootstrap standard errors.

}
\usage{
boot.sd1(x, weight,  kp, nsim, boot.index = c("r.hc", "r.is"), gamma)
}
\arguments{
\item{x}{income vector}
\item{weight}{vector of weights }
\item{kp}{multiple of the median income (k) or order of quantile (p)}
\item{nsim}{the number of replications}
\item{boot.index}{the index for which the error is estimated}
\item{gamma}{confidence level}
}

\details{The function uses quantile method of calculating bootstrap confidence intervals.
}
\value{
\item{se.r}{the bootstrap error}
\item{summary}{bootstrap summary}
\item{boot.ind}{bootstraped sample of index}
}
\references{
Brzezinski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299.}
\author{Alicja Wolny-Dominiak}

\examples{
data(affluence)
affluence$weight <- rep(1, nrow(affluence))
boot.sd1(affluence$income, affluence$weight, 0.9, 10, "r.is", 0.95)
boot.sd1(affluence$income, affluence$weight, 2, 10, "r.hc", 0.95)
}
\keyword{indboot}
