\name{vsim}
\alias{vsim}
\title{Simulation of V-approximation}
\usage{
vsim(vcoeff, adata, wval = NULL)
}
\arguments{
\item{vcoeff}{An approximation result from \code{varpox} function}

\item{adata}{A data.frame or matrix of [stock]=[\eqn{\mathbf{S}}]}

\item{wval}{(Optional for \code{plotgen}) An array of \eqn{W}-value}
}
\value{
A list of simulation resuts: shadow (accounting) prices, inclusive wealth, Value function,\cr
stock, and W values. Use \code{results$item} (or \code{results[["item"]]}) to import each result item.
\item{shadowp}{Shadow price}
\item{iweach}{Inclusive wealth for each stock for multi-stock case}
\item{vfun}{Value function}
\item{stock}{Stock}
\item{wval}{W-value if \code{wval} is provided}
}
\description{
The function provides the V-approximation simulation by adopting the results of \code{vaprox}. Available for multiple stock problems.
}
\details{
Let \eqn{\hat{\beta}} be the approximation coefficent from the results of \code{vaprox} function.
The estimated shadow (accounting) price of \eqn{i}-th stock over the given approximation intervals of \eqn{s_{i} \in [a_{i},b_{i}]},
\eqn{\hat{p}_{i}} can be calcuated as:\cr
\cr
\eqn{\hat{p}_{i} = \mathbf{\mu}(\mathbf{S})\mathbf{\hat{\beta}}} where \eqn{\mathbf{\mu}(\mathbf{S})} Chebyshev polynomial basis.\cr
\cr
The value function is:\cr
\cr
\eqn{\hat{V} = \delta \mathbf{\mu}(\mathbf{S})\mathbf{\hat{\beta}}}.\cr
\cr
For more detils see Fenichel and Abbott (2014), Fenichel et al. (2016a), Fenichel et al. (2016b), and Yun et al. (2017).
}
\examples{
## 1-D Reef-fish example: see Fenichel and Abbott (2014)
data("GOM")
nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
simuDataV <- cbind(nodes,sdot(nodes,param),profit(nodes,param))
Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
vC <- vaprox(Aspace,simuDataV)
# Note vcol function requries a data.frame or matrix!
GOMSimV <- vsim(vC,as.matrix(simuDataV[,1],ncol=1),profit(nodes,param))

# plot shadow (accounting) price: Figure 4 in Fenichel and Abbott (2014)
plotgen(GOMSimV, xlabel="Stock size, s", ylabel="Shadow price")

## 2-D Prey-Predator example
data("lvdata")
aproxdeg <- c(20,20)
lower <- c(0.1,0.1)
upper <- c(1.5,1.5)
delta <- 0.03
lvspace <- aproxdef(aproxdeg,lower,upper,delta)
lvaproxc <- vaprox(lvspace,lvaproxdata)
lvsim <- vsim(lvaproxc,lvsimdata.time[,2:3])

# plot Biomass
plot(lvsimdata.time[,1], lvsimdata.time[,2], type='l', lwd=2, col="blue",
     xlab="Time",
     ylab="Biomass")
lines(lvsimdata.time[,1], lvsimdata.time[,3], lwd=2, col="red")
legend("topright", c("Prey", "Predator"), col=c("blue", "red"),
       lty=c(1,1), lwd=c(2,2), bty="n")

# plot shadow (accounting) prices
plot(lvsimdata.time[,1],lvsim[["shadowp"]][,1],type='l', lwd=2, col="blue",
     ylim = c(-5,7),
     xlab="Time",
     ylab="Shadow price")
lines(lvsimdata.time[,1],lvsim[["shadowp"]][,2], lwd=2, col="red")
legend("topright", c("Prey", "Predator"), col=c("blue", "red"),
       lty=c(1,1), lwd=c(2,2), bty="n")

# plot inclusive weath and value function
plot(lvsimdata.time[,1],lvsim[["iw"]],type='l', lwd=2, col="blue",
     ylim = c(-0.5,1.2),
     xlab="Time",
     ylab="Inclusive Wealth / Value Function ($)")
lines(lvsimdata.time[,1],lvsim[["vfun"]], lwd=2, col="red")
legend("topright", c("Inclusive Wealth", "Value Function"),
       col=c("blue", "red"), lty=c(1,1), lwd=c(2,2), bty="n")
}
\references{
Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
             \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
Fenichel, Eli P., Joshua K. Abbott, Jude Bayham, Whitney Boone, Erin M. K. Haacker, and Lisa Pfeiffer. (2016a) "\href{http://www.pnas.org/content/113/9/2382}{Measuring the Value of Groundwater and Other Forms of Natural Capital}."
             \emph{Proceedings of the National Academy of Sciences}.113:2382-2387.\cr
Fenichel, Eli P., Simon A. Levin, Bonnie McCay, Kevin St. Martin, Joshua K. Abbott, and Malin L. Pinsky. (2016b) "\href{http://www.nature.com/nclimate/index.html}{Wealth Reallocation and Sustainability under Climate Change}."
             \emph{Nature Climate change}.6:237-244.\cr
Yun, Seong Do, Barbara Hutniczak, Joshua K. Abbott, and Eli P. Fenichel. (2017) "Ecosystem Based Management and the Welath of Ecosystems" \emph{Proceedings of the National Academy of Sciences}. (forthcoming).
}
\seealso{
\code{\link{aproxdef}, \link{vsim}}
}
