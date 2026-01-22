\name{pdotaprox}
\alias{pdotaprox}
\title{Calculating Pdot-approximation coefficients}
\usage{
pdotaprox(aproxspace, stock, sdot, dsdotds, dsdotdss, dwds, dwdss)
}
\arguments{
\item{aproxspace}{An approximation space defined by \code{aproxdef} function}

\item{stock}{An array of stock, \eqn{s}}

\item{sdot}{An array of ds/dt, \eqn{\dot{s}=\frac{ds}{dt}}}

\item{dsdotds}{An array of d(sdot)/ds, \eqn{\frac{d \dot{s}}{d s}}}

\item{dsdotdss}{An array of d/ds(d(sdot)/ds), \eqn{ \frac{d}{ds} \left( \frac{d \dot{s}}{ds} \right)}}

\item{dwds}{An array of dw/ds, \eqn{\frac{dW}{ds}}}

\item{dwdss}{An array of d/ds(dw/ds), \eqn{\frac{d}{ds} \left( \frac{dW}{ds} \right)}}
}
\value{
A list of approximation results: deg, lb, ub, delta, and coefficients. Use \code{results$item} \cr (or \code{results[["item"]]}) to import each result item.
\item{degree}{degree of Chebyshev polynomial}
\item{lowerB}{lower bound of Chebyshev nodes}
\item{upperB}{upper bound of Chebyshev nodes}
\item{delta}{discount rate}
\item{coefficient}{Chebyshev polynomial coefficients}
}
\description{
The function provides the Pdot-approximation coefficients of the defined Chebyshev polynomials in \code{aproxdef}.
             For now, only unidimensional case is developed.
}
\details{
The Pdot-approximation is finding the shadow price of a stock, \eqn{p} from the relation:\cr
\cr
\eqn{p(s) = \frac{W_{s}(s) + \dot{p}(s)}{\delta - \dot{s}_{s}}}, \cr
\cr
where \eqn{W_{s} = \frac{dW}{ds}}, \eqn{ \dot{p}(s) = \frac{dp}{ds}},
\eqn{\dot{s}_{s} = \frac{d\dot{s}}{ds} }, and \eqn{\delta} is the given discount rate.\cr
\cr
In order to operationalize this approach, we take the time derivative of this expression:\cr
\cr
\eqn{ \dot{p} = \frac{ \left( \left(W_{ss}\dot{s} + \ddot{p} \right) \left( \delta - \dot{s}_{s} \right) +
   \left( W_{s} + \dot{p} \right) \left(\dot{s}_{ss} \dot{s} \right)  \right)  }
   { \left( \delta - \dot{s}_{s} \right)^{2} } } \cr
\cr
Consider approximation \eqn{ \dot{p}(s) = \mathbf{\mu}(s)\mathbf{\beta}}, \eqn{\mathbf{\mu}(s)}
is Chebyshev polynomials and \eqn{\mathbf{\beta}} is their coeffcients.
Then, \eqn{ \ddot{p} = \frac{ d \dot{p}}{ds} \frac{ds}{dt} = diag (\dot{s}) \mathbf{\mu}_{s}(s) \mathbf{\beta}} by the orthogonality of Chebyshev basis.
Adopting the properties above, we can get the unknown coefficient vector \eqn{\beta} from:\cr
\cr
\eqn{ \mathbf{\mu \beta} =  diag \left( \delta - \dot{s}_{s} \right)^{-2}
   \left[ \left(W_{ss}\dot{s} + diag (\dot{s}) \mathbf{\mu}_{s} \mathbf{\beta} \right)\left( \delta - \dot{s}_{s} \right) +
   diag \left(\dot{s}_{ss} \dot{s} \right) \left( W_{s} + \mathbf{\mu \beta} \right) \right] }, and \cr
\cr
\eqn{\mathbf{\beta} = \left[ diag \left( \delta - \dot{s}_{s} \right)^{2} \mathbf{\mu} - diag \left( \dot{s}\left( \delta - \dot{s}_{s} \right) \right) \mathbf{\mu}_{s}
    - diag (\dot{s}_{ss} \dot{s} ) \mathbf{\mu}   \right]^{-1}
    \left( W_{ss} \dot{s} \left( \delta - \dot{s}_{s} \right) + W_{s} \dot{s}_{ss} \dot{s} \right) }.  \cr
\cr
If we suppose \eqn{ A = \left[ diag \left( \delta - \dot{s}_{s} \right)^{2} \mathbf{\mu} - diag \left( \dot{s}\left( \delta - \dot{s}_{s} \right) \right) \mathbf{\mu}_{s}
    - diag (\dot{s}_{ss} \dot{s} ) \mathbf{\mu}    \right] } and \cr
\eqn{ B = \left( W_{ss} \dot{s} \left( \delta - \dot{s}_{s} \right) + W_{s} \dot{s}_{ss} \dot{s} \right) },
then over-determined case can be calculated:\cr
\cr
\eqn{ \mathbf{\beta} = \left( A^{T}A \right)^{-1} A^{T}B }.\cr
\cr
For more detils see \href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Fenichel and Abbott (2014)}.
}
\examples{
## 1-D Reef-fish example: see Fenichel and Abbott (2014)
data("GOM")
nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
simuDataPdot <- cbind(nodes,sdot(nodes,param),
                      dsdotds(nodes,param),dsdotdss(nodes,param),
                      dwds(nodes,param),dwdss(nodes,param))
Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
pdotC <- pdotaprox(Aspace,simuDataPdot[,1],simuDataPdot[,2],
                   simuDataPdot[,3],simuDataPdot[,4],
                   simuDataPdot[,5],simuDataPdot[,6])
}
\references{
Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
             \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
}
\seealso{
\code{\link{aproxdef}, \link{pdotsim}}
}
