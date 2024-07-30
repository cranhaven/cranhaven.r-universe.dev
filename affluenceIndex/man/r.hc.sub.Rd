\name{r.hc.sub}
\alias{r.hc.sub}
\title{
Richness headcount ratio in subpopulation}
\description{Computes the richness headcount ratio in subpopulation.
}

\usage{
r.hc.sub(x.sub, x, weight.sub, weight, k)
}

\arguments{
\item{x.sub}{income vector of subpopulation}
\item{x}{income vector of population}
\item{weight.sub}{weight vector of subpopulation}
\item{weight}{weight vector of population}
\item{k}{multiple of the median income}
}

\value{
\item{count.rich}{the number of the rich}
\item{r.hc}{the value of the index}
}

\references{
1. Brzezinski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299. \cr
2. Saczewska-Piotrowska A. (2015) Identification of determinants of income richness using logistic regression model. 
\emph{Zarzadzanie i Finanse. Journal of Management and Finance}, 4, Part 2, pp. 241-259 (in Polish).
}

\author{Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska}

\seealso{\code{\link{r.hc}}
}

\examples{
data(affluence)
affluence$weight <- rep(1, nrow(affluence))
aff.sub <- subset(affluence, education == 2)
r.hc.sub(aff.sub$income, affluence$income, aff.sub$weight, affluence$weight, 3)

}
\keyword{ind}
