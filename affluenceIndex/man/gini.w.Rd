\name{gini.w}
\alias{gini.w}

\title{
Gini coefficient}

\usage{
gini.w(x, weight)
}

\description{Computes the Gini coefficient.}

\arguments{
  \item{x}{income vector of population}
  \item{weight}{vector of weights}
}
\details{
The Gini coefficient is the most popular measure of income inequality. The formula taking into account the weights of income \eqn{w_1,w_2,...,w_n} is given by:
\deqn{G_w = \frac{\sum_{i=1}^nw_i\sum_{j=1}^n w_j|x_i-x_j|}{2(\sum_{i=1}^nw_i)^2\mu_w},}
where \eqn{x_i,x_j} are incomes of individuals \eqn{i} and {j}, respectively, \eqn{n} is the number of individuals, \eqn{\mu_w} is the mean income. The Gini coefficient ranges between 1 (perfect equality) and 1 (perfect inequality).
}
\value{
\item{GG}{the value of coefficient}
}
\references{
1. Creedy J. (2015). A note on computing the Gini inequality measure with weighted data. Workin Paper No. 3, Victoria University of Wellington. \cr
2. Lerman R.I., Yitzhaki S. (1989) Improving the accuracy of estimates of Gini coefficients. \emph{Journal of Econometrics}, 42(1), pp. 43-47.
}
\author{
Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska}

\examples{
data(affluence)
gini.w(affluence$income, affluence$hs_size)
}
\keyword{ineq}