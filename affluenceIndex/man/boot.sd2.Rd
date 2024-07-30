\name{boot.sd2}
\alias{boot.sd2}
\title{Bootstrap standard error 2
}
\description{Calculates the bootstrap standard errors.

}
\usage{
boot.sd2(x, weight, k, alpha, nsim, boot.index = c("r.cha", "r.fgt"), gamma)
}
\arguments{
\item{x}{income vector}
\item{weight}{weight vector of population}
\item{k}{multiple of the median income}
\item{alpha}{parameter of the index: \code{alpha} > 0 for "\code{r.cha}", \code{alpha} > 1 for "\code{r.fgt}"}
\item{nsim}{the number of replications}
\item{boot.index}{the index for which the error is estimated}
\item{gamma}{confidence level}

}
\details{The function uses quantile method of calculating bootstrap confidence intervals.
}
\value{
\item{se.r}{the bootstrap error}
\item{summary}{bootstrap summary}
\item{boot.ind}{vector of bootstraped index}
}
\references{
Brzezinski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299.}
\author{Alicja Wolny-Dominiak}

\examples{
data(affluence)
boot.sd2(affluence$income, weight = NULL, 2, 2, 10, "r.cha", 0.95)
boot.sd2(affluence$income, weight = NULL, 2, 2, 10, "r.fgt", 0.95)
}
\keyword{indboot}