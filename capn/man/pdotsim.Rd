\name{pdotsim}
\alias{pdotsim}
\title{Simulation of Pdot-approximation}
\usage{
pdotsim(pdotcoeff, stock, sdot, dsdotds, wval, dwds)
}
\arguments{
\item{pdotcoeff}{An approximation result from \code{pdotaprox} function}

\item{stock}{An array of stock}

\item{sdot}{An array of ds/dt, \eqn{\dot{s}=\frac{ds}{dt}}}

\item{dsdotds}{An array of d(sdot)/ds, \eqn{\frac{d \dot{s}}{d s}}}

\item{wval}{An array of \eqn{W}-value}

\item{dwds}{An array of dw/ds, \eqn{\frac{dW}{ds}}}
}
\value{
A list of approximation resuts: shadow (accounting) prices, inclusive wealth, and value function, stock, and W values. Use \code{results$item} (or \code{results[["item"]]}) to import each result item.
\item{shadowp}{Shadow price}
\item{vfun}{Value function}
\item{stock}{Stock}
\item{wval}{W-value}
}
\description{
The function provides the Pdot-approximation simulation.
}
\details{
Let \eqn{\hat{\beta}} be the vector of approximation coefficents from the results of \code{pdotaprox} function.
The estimated shadow price (accounting) price of stock over the given approximation interval of
\eqn{s \in [a,b]}, \eqn{\hat{p}} can be calculated as:\cr
\cr
\eqn{\hat{p} = \frac{ W_{s} + \mathbf{\mu \beta} }{ \delta - \dot{s}_{s} } }.\cr
\cr
The estimated value function is:\cr
\cr
\eqn{ \hat{V} = \frac{1}{\delta} \left( W + \hat{p} \dot{s} \right)  }.\cr
\cr
For more detils see \href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Fenichel and Abbott (2014)} and \href{http://www.pnas.org/content/113/9/2382}{Fenichel et al. (2016)}.
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
GOMSimPdot <- pdotsim(pdotC,simuDataPdot[,1],simuDataPdot[,2],simuDataPdot[,3],
                      profit(nodes,param),simuDataPdot[,5])
# Shadow Price
plotgen(GOMSimPdot, xlabel="Stock size, s", ylabel="Shadow price")

# Value function and profit
plotgen(GOMSimPdot,ftype="vw",
        xlabel="Stock size, s",
        ylabel=c("Value Function","Profit"))
}
\references{
Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
             \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
Fenichel, Eli P., Joshua K. Abbott, Jude Bayham, Whitney Boone, Erin M. K. Haacker, and Lisa Pfeiffer. (2016) "\href{http://www.pnas.org/content/113/9/2382}{Measuring the Value of Groundwater and Other Forms of Natural Capital}."
             \emph{Proceedings of the National Academy of Sciences }.113:2382-2387.
}
\seealso{
\code{\link{pdotaprox}}
}
