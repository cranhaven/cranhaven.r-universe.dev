\name{r.is.sub}
\alias{r.is.sub}
\title{Income share of the top p \% in subpopulation
}
\description{Computes income share of the top p \% in subpopulation.
}

\usage{
r.is.sub(x.sub, x, weight.sub, weight, p)
}

\arguments{
\item{x.sub}{income vector of subpopulation}
\item{x}{income vector of population}
\item{weight.sub}{weight vector of subpopulation}
\item{weight}{weight vector of population}
\item{p}{the order of quantile. Must be in [0,1] as probability}
}

\value{
\item{r.2}{the value of index}
}

\references{
Brzezinski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299.
}

\author{Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska}

\seealso{\code{\link{r.is}}
}

\examples{
data(affluence)
affluence$weight <- rep(1, nrow(affluence))
aff.sub <- subset(affluence, education == 2)
r.is.sub(aff.sub$income, affluence$income, aff.sub$weight, affluence$weight, 0.9)
}
\keyword{ind}
