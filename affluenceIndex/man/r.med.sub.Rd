\name{r.med.sub}
\alias{r.med.sub}
\title{Average affluence gap in subpopulation
}
\description{Computes the average affluence gap in subpopulation.
}

\usage{
r.med.sub(x.sub, x, weight.sub, weight, k)
}

\arguments{
\item{x}{income vector of subpopulation}
\item{x.sub}{income vector of population}
\item{weight.sub}{weight vector of subpopulation}
\item{weight}{weight vector of population}
\item{k}{multiple of the median income}
}

\value{
\item{gap}{the gap value}
}

\references{
Medeiros M. (2006) The rich and the poor: the construction of an affluence line from the poverty line. \emph{Social Indicators Research}, 78, pp. 1-18.
}

\author{Alicja Wolny-Dominiak
}

\seealso{\code{\link{r.med}}
}

\examples{
data(affluence)
affluence$weight <- rep(1, nrow(affluence))
aff.sub <- subset(affluence, education == 2)
r.med.sub(aff.sub$income, affluence$income, aff.sub$weight, affluence$weight, 2)
}
\keyword{ind}