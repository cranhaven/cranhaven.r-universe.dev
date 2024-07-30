\name{boot.sd1.sub}
\alias{boot.sd1.sub}
\title{Bootstrap standard error 1
}
\description{The estimation of bootstrap standard error of affluence index in subpopulation.
}

\usage{
boot.sd1.sub(x.sub, x, weight.sub, weight, kp, nsim, boot.index=c("r.hc", "r.is"), gamma)
}
\arguments{
\item{x}{income vector of subpopulation}
\item{x.sub}{income vector of population}
\item{weight.sub}{weight vector of subpopulation}
\item{weight}{weight vector of population}
\item{kp}{multiple of the median income (k) or order of quantile (p)}
\item{nsim}{the number of replications}
\item{boot.index}{the index for which the error is estimated}
\item{gamma}{confidence level}
}

\details{The function uses quantile method of calculating bootstrap confidence intervals.
}
\value{
\item{se.r}{the bootstrap error of the affluence index}
\item{summary}{bootstrap summary}
\item{boot.ind}{bootstraped sample of index}
}
\references{
Brzezinski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299.}
\author{Alicja Wolny-Dominiak}

\examples{
data(affluence)
affluence$weight <- rep(1, nrow(affluence))
aff.sub <- subset(affluence, education == 2)

boot.sd1.sub(aff.sub$income, affluence$income, aff.sub$weight, affluence$weight, 
             0.9, 10, "r.is", 0.95)

boot.sd1.sub(aff.sub$income, affluence$income, aff.sub$weight, affluence$weight, 
             0.9, 10, "r.hc", 0.95)
}
\keyword{indboot}