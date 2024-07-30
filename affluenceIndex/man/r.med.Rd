\name{r.med}
\alias{r.med}
\title{Average affluence gap
}
\description{Computes the average affluence gap of population.
}
\usage{r.med(x, weight, k)
}
\arguments{
\item{x}{income vector}
\item{weight}{vector of weights}
\item{k}{multiple of the median income}
}

\details{Medeiros (2006) defined an average affluence gap. Weighted gap (with weights \eqn{w_1,w_2,...,w_n}) is given by:
\deqn{R^{Me}= \frac{\sum_{i=1}^n \max\{x_i-\rho_w,0\}w_i}{\sum_{i=1}^nw_i},}
where \eqn{x_i} is an income of individual \eqn{i}, \eqn{n} is the number of individuals, \eqn{\rho_w} is the richness line.
Medeiros' index is not standarized and is an absolute measure of richness.
}
\value{
\item{gap}{the value of the average affluence gap}
}
\references{
Medeiros M. (2006) The rich and the poor: the construction of an affluence line from the poverty line. \emph{Social Indicators Research}, 78, pp. 1-18.
}
\author{Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska
}
\examples{
data(affluence)
r.med(affluence$income, weight = NULL, 2)
}
\keyword{ind}