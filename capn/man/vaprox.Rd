\name{vaprox}
\alias{vaprox}
\title{Calculating V-approximation coefficients}
\usage{
vaprox(aproxspace, sdata)
}
\arguments{
\item{aproxspace}{An approximation space defined by \code{aproxdef} function}

\item{sdata}{A data.frame or matrix of [stock,sdot,benefit]=[\eqn{\mathbf{S}},\eqn{\mathbf{\dot{S}}},\eqn{W}]}
}
\value{
A list of approximation resuts: deg, lb, ub, delta, and coefficients. Use \code{results$item} \cr (or \code{results[["item"]]}) to import each result item.
\item{degree}{degree of Chebyshev polynomial}
\item{lowerB}{lower bound of Chebyshev nodes}
\item{upperB}{upper bound of Chebyshev nodes}
\item{delta}{discount rate}
\item{coefficient}{Chebyshev polynomial coefficients}
}
\description{
The function provides the V-approximation coefficients of the defined Chebyshev polynomials in \code{aproxdef}.
}
\details{
The V-approximation is finding the shadow price of \eqn{i}-th stock, \eqn{p_{i}} for \eqn{i=1,\cdots,d}
from the relation:\cr
\cr
\eqn{\delta V = W(\mathbf{S}) + p_{1}\dot{s}_{1} + p_{2}\dot{s}_{2} + \cdots + p_{d}\dot{s}_{d}},\cr
\cr
where \eqn{\delta} is the given discount rate, \eqn{V} is the intertemporal welfare function, \eqn{\mathbf{S} = (s_{1}, s_{2}, \cdots, s_{d})} is a vector of stocks, \eqn{W(\mathbf{S})} is the net benefits accruing to society,
and \eqn{\dot{s}_{i}} is the growth of stock \eqn{s_{i}}. By the definition of the shadow price, we know:\cr
\cr
\eqn{p_{i} = \frac{\partial V}{\partial s_{i}}}.\cr
\cr
Consider approximation \eqn{V(\mathbf{S}) = \mathbf{\mu}(\mathbf{S})\mathbf{\beta}}, \eqn{\mathbf{\mu}(\mathbf{S})}
is Chebyshev polynomials and \eqn{\mathbf{\beta}} is their coeffcients.
Then, \eqn{p_{i} = \mathbf{\mu}_{s_{i}}(\mathbf{S})\mathbf{\beta}} by the orthogonality of Chebyshev basis.
Adopting the properties above, we can get the unknown coefficient vector \eqn{\beta} from:\cr
\cr
\eqn{\delta \mathbf{\mu}(\mathbf{S})\mathbf{\beta} = W(\mathbf{S}) + \displaystyle \sum_{i=1}^{d} diag (\dot{s}_{i}) \mathbf{\mu}_{s_{i}}(\mathbf{S})\mathbf{\beta}}, and thus,\cr
\cr
\eqn{\beta = \left( \delta \mathbf{\mu}(\mathbf{S}) - \displaystyle \sum_{i=1}^{d} diag (\dot{s}_{i}) \mathbf{\mu}_{s_{i}}(\mathbf{S}) \right)^{-1} W(\mathbf{S}) }.\cr
\cr
In a case of over-determined (more nodes than approaximation degrees),\cr
\cr
\eqn{\beta = \left( \left( \delta \mathbf{\mu}(\mathbf{S}) - \displaystyle diag (\dot{s}_{i}) \sum_{i=1}^{d} \mathbf{\mu}_{s_{i}}(\mathbf{S}) \right)^{T}
\left( \delta \mathbf{\mu}(\mathbf{S}) - \displaystyle \sum_{i=1}^{d} diag (\dot{s}_{i}) \mathbf{\mu}_{s_{i}}(\mathbf{S}) \right) \right)^{-1}} \cr
\eqn{\times \left( \delta \mathbf{\mu}(\mathbf{S}) - \displaystyle \sum_{i=1}^{d} diag (\dot{s}_{i}) \mathbf{\mu}_{s_{i}}(\mathbf{S}) \right)^{T} W(\mathbf{S}) }.\cr
\cr
For more detils see Fenichel and Abbott (2014), Fenichel et al. (2016), and Yun et al. (2017).
}
\examples{
## 1-D Reef-fish example: see Fenichel and Abbott (2014)
data("GOM")
nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
simuDataV <- cbind(nodes,sdot(nodes,param),profit(nodes,param))
Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
vC <- vaprox(Aspace,simuDataV)

## 2-D Prey-Predator example
data("lvdata")
aproxdeg <- c(20,20)
lower <- c(0.1,0.1)
upper <- c(1.5,1.5)
delta <- 0.03
lvspace <- aproxdef(aproxdeg,lower,upper,delta)
vaproxc <- vaprox(lvspace,lvaproxdata)
}
\references{
Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
             \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
Fenichel, Eli P., Joshua K. Abbott, Jude Bayham, Whitney Boone, Erin M. K. Haacker, and Lisa Pfeiffer. (2016) "\href{http://www.pnas.org/content/113/9/2382}{Measuring the Value of Groundwater and Other Forms of Natural Capital}."
             \emph{Proceedings of the National Academy of Sciences}.113:2382-2387.\cr
Yun, Seong Do, Barbara Hutniczak, Joshua K. Abbott, and Eli P. Fenichel. (2017) "Ecosystem Based Management and the Welath of Ecosystems" \emph{Proceedings of the National Academy of Sciences}. (forthcoming).
}
\seealso{
\code{\link{aproxdef}, \link{vsim}}
}
