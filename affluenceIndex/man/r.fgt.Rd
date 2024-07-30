\name{r.fgt}
\alias{r.fgt}
\title{Convex measure of affluence
}
\description{Computes the measure of affluence analogous to the convex version of Foster, Greer and Thorbecke (1984) family of poverty indices.
}
\usage{
r.fgt(x, weight, k, alpha)
}
\arguments{
\item{x}{the income vector}
\item{weight}{vector of weights}
\item{k}{multiple of the median income}
\item{alpha}{parameter of the index: \code{alpha} > 1}
}
\details{Peichl et. al (2008) defined an affluence index. Weighted index (with weights \eqn{w_1,w_2,...,w_n}) is given by: 
\deqn{R_{\alpha}^{FGT,T2}(\mathbf{x},\mathbf{w},\rho_w)=\frac{\sum_{i=1}^{n} \left( \frac{x_i - \rho_w}{\rho_w}\right)^{\alpha}\mathbf{1}_{x_i>\rho_w}w_i}{\sum_{i=1}^{n}w_i},\alpha>1,}
where \eqn{x_i} is an income of individual \eqn{i}, \eqn{n} is the number of individuals, \eqn{\rho_w} is the richness line, \eqn{\boldsymbol{1}_{(\cdot)}} 
denotes the indicator function, which is equal to 1 when its argument is true and 0 otherwise.
Index satisfies transfer axiom \eqn{T2} (convex): a richness index should decrease when a rank-preserving progressive transfer between two rich individuals takes place.
}
\value{
\item{r}{values of the sum in the index formula}
\item{r.fgt}{the value of index}
}
\references{
1. Foster J.E., Greer J., Thorbecke E. (1984) A class of decomposable poverty measures. \emph{Econometrica}, 52, pp. 761-766. \cr
2. Peichl A., Schaefer T., Scheicher C. (2008) Measuring richness and poverty - A micro data application to Europe and Germany. IZA Discussion Paper No. 3790, Institute for the Study of Labor (IZA).
}
\author{Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska}


\examples{
data(affluence)
r.fgt(affluence$income, weight = NULL, 2, 1)
}
\keyword{ind}
