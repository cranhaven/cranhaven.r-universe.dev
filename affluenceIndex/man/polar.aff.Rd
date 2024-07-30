\name{polar.aff}
\alias{polar.aff}
\title{Wolfson polarization index
}
\description{Computes the Wolfson polarization index.
}
\usage{
polar.aff(x, weight)
}
\arguments{
\item{x}{the income vector}
\item{weight}{vector of weights}
}
\details{
Standard inequality measures do not give any information about polarization. A more polarized income distribution 
is one that has relatively fewer middle income class and more low- and/or high-income households (Alichi et al. 2016). Low income class is
very often identified with poverty and high-income class with richness. One of the measures of polarization is the Wolfson polarization index (Wolfson 1994).
Weighted version of this index is given by:
\deqn{P_w= 2 \left( 2T-G_w \right) \frac{\mu_w}{\rho_w},}
where \eqn{T} is the difference between 0.5 and the income share of bottom half of the population, \eqn{G_w} is the Gini coefficient,
\eqn{\mu_w} is the mean income, \eqn{\rho_w} is the median income.
}

\value{
\item{Pw}{the value of index}
\item{TT}{the difference between 0.5 and the income share of bottom half of the population}
}


\references{
1. Alichi A., Kantenga K., Sole J. (2016) Income polarization in the United States. IMF Working Paper, WP/16/121.\cr
2. Wolfson M.C. (1994) When inequalities diverge, \emph{The American Economic Review}, 84, pp. 353-358.
}
\author{Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska
}
\examples{
data(affluence)
polar.aff(affluence$income, weight = NULL)
}
\keyword{polar}