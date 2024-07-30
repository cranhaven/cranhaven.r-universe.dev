\name{r.cha}
\alias{r.cha}
\title{Concave measure of affluence
}
\description{Computes the measure of affluence analogous to the poverty index of Chakravarty (1983).
}
\usage{
r.cha(x, weight, k, beta)
}

\arguments{
\item{x}{the income vector}
\item{weight}{vector of weights}
\item{k}{multiple of the median income}
\item{beta}{parameter of the index: \code{beta} > 0}
}
\details{Peichl et. al (2008) defined an affluence index. Weighted index (with weights \eqn{w_1,w_2,...,w_n}) is given by:
\deqn{R^{CHA}_{\beta}(\boldsymbol{x},\boldsymbol{w},\rho_w) = \frac{\sum_{i=1}^n(1-(\frac{\rho_w}{x_i})^\beta)\boldsymbol{1}_{x_i > \rho_w}w_i}{\sum_{i=1}^n{w_i}}, \beta > 0,}
where \eqn{x_i} is an income of individual \eqn{i}, \eqn{n} is the number of individuals, \eqn{\rho_w} is the richness line, 
\eqn{\boldsymbol{1}_{(\cdot)}} denotes the indicator function, which is equal to 1 when its argument is true and 0 otherwise.
Index satisfies transfer axiom \eqn{T1} (concave): a richness index should increase when a rank-preserving progressive transfer between two rich individuals takes place.
}
\value{
\item{r}{elements of the sum in the index formula}
\item{r.cha}{the value of index}
}
\references{
1. Chakravarty S.R. (1983) A new index of poverty. \emph{Mathematical Social Sciences}, 6, pp. 307-313. \cr
2. Peichl A., Schaefer T., Scheicher C. (2008) Measuring richness and poverty - A micro data application to Europe and Germany. IZA Discussion Paper No. 3790, Institute for the Study of Labor (IZA).
}

\author{Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska}

\examples{
data(affluence)
r.cha(affluence$income, weight = NULL, 2, 2)
}
\keyword{ind}

