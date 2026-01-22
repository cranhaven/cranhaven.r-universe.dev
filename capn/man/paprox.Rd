\name{paprox}
\alias{paprox}
\title{Calculating P-approximation coefficients}
\usage{
paprox(aproxspace, stock, sdot, dsdotds, dwds)
}
\arguments{
\item{aproxspace}{An approximation space defined by \code{aproxdef} function}

\item{stock}{An array of stock, \eqn{s}}

\item{sdot}{An array of ds/dt, \eqn{\dot{s}=\frac{ds}{dt}}}

\item{dsdotds}{An array of d(sdot)/ds, \eqn{\frac{d \dot{s}}{d s}}}

\item{dwds}{An array of dw/ds, \eqn{\frac{dW}{ds}}}
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
The function provides the P-approximation coefficients of the defined Chebyshev polynomials in \code{aproxdef}.
             For now, only unidimensional case is developed.
}
\details{
The P-approximation is finding the shadow price of a stock, \eqn{p} from the relation:\cr
\cr
\eqn{p(s) = \frac{W_{s}(s) + \dot{p}(s)}{\delta - \dot{s}_{s}}}, \cr
\cr
where \eqn{W_{s} = \frac{dW}{ds}}, \eqn{ \dot{p}(s) = \frac{dp}{ds}},
\eqn{\dot{s}_{s} = \frac{d\dot{s}}{ds} }, and \eqn{\delta} is the given discount rate.\cr
\cr
Consider approximation \eqn{p(s) = \mathbf{\mu}(s)\mathbf{\beta}}, \eqn{\mathbf{\mu}(s)}
is Chebyshev polynomials and \eqn{\mathbf{\beta}} is their coeffcients.
Then, \eqn{\dot{p} = diag (\dot{s}) \mathbf{\mu}_{s}(s)\mathbf{\beta}} by the orthogonality of Chebyshev basis.
Adopting the properties above, we can get the unknown coefficient vector \eqn{\beta} from:\cr
\cr
\eqn{\mathbf{\mu}\mathbf{\beta} = diag \left( \delta - \dot{s}_{s} \right)^{-1} \left( W_{s} + diag (\dot{s}) \mathbf{\mu}_{s} \mathbf{\beta} \right) }, and thus, \cr
\cr
\eqn{\mathbf{\beta} = \left( diag \left( \delta - \dot{s}_{s} \right) \mathbf{\mu} - diag (\dot{s}) \mathbf{\mu}_{s} \right)^{-1} W_{s} }.\cr
\cr
In a case of over-determined (more nodes than approaximation degrees),\cr
\cr
\eqn{\left( \left( diag \left( \delta - \dot{s}_{s} \right) \mathbf{\mu} - diag (\dot{s}) \mathbf{\mu}_{s} \right)^{T}
\left( diag \left( \delta - \dot{s}_{s} \right) \mathbf{\mu} - diag (\dot{s}) \mathbf{\mu}_{s} \right) \right)^{-1}
\left( diag \left( \delta - \dot{s}_{s} \right) \mathbf{\mu} - diag (\dot{s}) \mathbf{\mu}_{s} \right)^{T} W_{s}} \cr
\cr
For more detils see \href{http://www.pnas.org/content/113/9/2382}{Fenichel et al. (2016)}.
}
\examples{
## 1-D Reef-fish example: see Fenichel and Abbott (2014)
data("GOM")
nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
simuDataP <- cbind(nodes,sdot(nodes,param),
                   dsdotds(nodes,param),dwds(nodes,param))
Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
pC <- paprox(Aspace,simuDataP[,1],simuDataP[,2],
             simuDataP[,3],simuDataP[,4])
}
\references{
Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
             \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
Fenichel, Eli P., Joshua K. Abbott, Jude Bayham, Whitney Boone, Erin M. K. Haacker, and Lisa Pfeiffer. (2016) "\href{http://www.pnas.org/content/113/9/2382}{Measuring the Value of Groundwater and Other Forms of Natural Capital}."
             \emph{Proceedings of the National Academy of Sciences }.113:2382-2387.
}
\seealso{
\code{\link{aproxdef},\link{psim}}
}
