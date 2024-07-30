\name{S90S40}
\alias{S90S40}

\title{
Palma index}
\description{
Computes the Palma index (also known as S90/S40 ratio)}

\usage{
S90S40(x, weight)
}
\arguments{
  \item{x}{income vector}
  \item{weight}{vector of weights}
}
\details{
The Palma index is the ratio between the income share of the top \eqn{10\%} and the bottom \eqn{40\%}. The weighted Palma index (with weights \eqn{w_1,w_2,...,w_n}) is given by:
\deqn{S90/S40 = \frac{\sum_{i=1}^n{x_iw_i}\boldsymbol{1}_{x_i>q_{w(0.9)}}}{\sum_{i=1}^n{x_iw_i}\boldsymbol{1}_{x_i \leq q_{w(0.4)}}},}
where \eqn{x_i} is an income of individual \eqn{i}, \eqn{n} is the number of individuals, \eqn{q_{w(0.9)},q_{w(0.4)}} are the 0.9 and 0.4 quantiles, respectively,
\eqn{\boldsymbol{1}_{(\cdot)}} denotes the indicator function, which is equal to 1 when its argument is true and 0 otherwise.
}
\value{
\item{S90S40}{the value of index}
}

\references{
Cobham A., Sumner A.(2013). Is it all about the tails? The Palma measure of income inequality. Working Paper No. 343, Center for Global Development}

\author{
Anna Saczewska-Piotrowska, Alicja Wolny-Dominiak}

\keyword{ineq}