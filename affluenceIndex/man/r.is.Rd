\name{r.is}
\alias{r.is}
\title{Income share of the top p \%
}
\description{Computes the income share of the top p \%.
}
\usage{
r.is(x, weight, p)
}

\arguments{
\item{x}{the vector of income}
\item{weight}{vector of weights}
\item{p}{the order of quantile. Must be in [0,1] as probability}
}
\details{The most popular measure of richness which takes a form (with weights \eqn{w_1,w_2,...,w_n}):
\deqn{R^{IS}(\boldsymbol{x},\boldsymbol{w},p) = \frac{\sum_{i=1}^n{x_iw_i}\boldsymbol{1}_{x_i>q_{w(1-p)}}}{\sum_{i=1}^n{x_iw_i}},}
where \eqn{q_{w(1-p)}} is the \eqn{(1-p)} quantile of the population and \eqn{\boldsymbol{1}_{(\cdot)}} 
denotes the indicator function, which is equal to 1 when its argument is true and 0 otherwise.
There is always \eqn{p} \% of rich individualsa in the population.
}

\value{
\item{r.2}{the value of index}
}

\references{
Brzezinski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299.
}

\author{Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska}

\examples{
data(affluence)
r.is(affluence$income, weight = NULL, 0.9)
}
\keyword{ind}