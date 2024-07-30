\name{r.hc}
\alias{r.hc}
\title{
Richness headcount ratio}
\description{Computes the richness headcount ratio.
}
\usage{
r.hc(x, weight, k)
}
\arguments{
\item{x}{the income vector}
\item{weight}{weight vector of population}
\item{k}{multiple of the median income}
}
\details{Richness headcount ratio is a proportion of the population with incomes above the affluence line. Weighted version (with weights \eqn{w_1,w_2,...,w_n}) 
of this ratio is given by:
\deqn{R^{HC}(\boldsymbol{x},\boldsymbol{w},\rho_w) = \frac{\sum_{i=1}^n \boldsymbol{1}_{x_i > \rho_w}w_i}{\sum_{i=1}^n{w_i}},}
where \eqn{x_i} is an income of individual \eqn{i}, \eqn{n} is the number of individuals, \eqn{\rho_w} is the richness line,\eqn{\boldsymbol{1}_{(\cdot)}} 
denotes the indicator function, which is equal to 1 when its argument is true and 0 otherwise.
}
\value{
\item{count.rich}{the number of the rich}
\item{r.hc}{the value of index}
}
\references{
1. Brzezinski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299. \cr
2. Saczewska-Piotrowska A. (2015) Identification of determinants of income richness using logistic regression model. 
\emph{Zarzadzanie i Finanse. Journal of Management and Finance}, 4, Part 2, pp. 241-259 (in Polish).
}

\author{Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska}

\examples{
data(affluence)
affluence$weight <- rep(1, nrow(affluence))
r.hc(affluence$income, affluence$weight, 3)

}
\keyword{ind}
