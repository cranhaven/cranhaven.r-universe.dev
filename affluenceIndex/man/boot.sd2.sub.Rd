\name{boot.sd2.sub}
\alias{boot.sd2.sub}
\title{Bootstrap standard error 2
}
\description{Calculates the bootstrap standard errors in subpopulation.

}
\usage{
boot.sd2.sub(x.sub,x,weight.sub,weight,k,alpha,nsim,boot.index=c("r.cha","r.fgt"),gamma)
}
\arguments{
\item{x}{income vector of subpopulation}
\item{x.sub}{income vector of population}
\item{weight.sub}{weight vector of subpopulation}
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
}
\references{
Brzezinski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299.}
\author{Alicja Wolny-Dominiak}

\examples{
data(affluence)
affluence$weight <- rep(1, nrow(affluence))
aff.sub <- subset(affluence, education == 2)

x <- aff.sub$income
boot.sd2.sub(x, affluence$income, aff.sub$weight, affluence$weight, 2, 2, 10, "r.cha", 0.95)
boot.sd2.sub(x, affluence$income, aff.sub$weight, affluence$weight, 2, 2, 10, "r.fgt", 0.95)
}
\keyword{indboot}