\name{psim}
\alias{psim}
\title{Simulation of P-approximation}
\usage{
psim(pcoeff, stock, wval = NULL, sdot = NULL)
}
\arguments{
\item{pcoeff}{An approximation result from \code{paprox} function}

\item{stock}{An array of stock variable}

\item{wval}{(Optional for vfun) An array of \eqn{W}-value (need \code{sdot} simultaneously)}

\item{sdot}{(Optional for vfun) An array of ds/dt, \eqn{\dot{s}=\frac{ds}{dt}} (need \code{W} simultaneously)}
}
\value{
A list of approximation resuts: shadow (accounting) prices, inclusive wealth, value function, stock, and W values. Use \code{results$item} (or \code{results[["item"]]}) to import each result item.
\item{shadowp}{Shadow price}
\item{vfun}{Value function}
\item{stock}{Stock}
\item{wval}{W-value if \code{wval} is provided}
}
\description{
The function provides the P-approximation simulation.
}
\details{
Let \eqn{\hat{\beta}} be the vector of approximation coefficents from the results of \code{paprox} function.
The estimated shadow price (accounting) price of stock over the given approximation interval of
\eqn{s \in [a,b]}, \eqn{\hat{p}} can be calculated as:\cr
\cr
\eqn{\hat{p} = \mathbf{\mu}(s) \mathbf{\hat{\beta}}}.\cr
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
simuDataP <- cbind(nodes,sdot(nodes,param),
                   dsdotds(nodes,param),dwds(nodes,param))
Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
pC <- paprox(Aspace,simuDataP[,1],simuDataP[,2],
             simuDataP[,3],simuDataP[,4])
GOMSimP <- psim(pC,simuDataP[,1],profit(nodes,param),simuDataP[,2])

# Shadow Price
plotgen(GOMSimP, xlabel="Stock size, s", ylabel="Shadow price")

# Value function and profit
plotgen(GOMSimP,ftype="vw",
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
\code{\link{aproxdef}, \link{paprox}}
}
