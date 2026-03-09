\name{carbonTurnover}
\alias{carbonTurnover}
\title{
A function for implementing RothC carbon turnover model in the soil
}
\description{
This function provides alternatives for implementing RothC carbon turnover model
}
\usage{
carbonTurnover(tt,clay,C0,In,Dr=1.44,effcts,solver)
}
\arguments{
  \item{tt}{
a vector of time in months or years for modelling carbon turnover in the soil
}
  \item{clay}{
Proportion of soil clay content in percent
}
  \item{C0}{
a vector containing five initial carbon pools in the five compartments C1 in DPM, C2 in RPM, C3 in BIO, C4 in HUM and C5 in IOM. They are arranged in the order C1,C2,C3,C4,C5 [C0=c(C1,C2,C3,C4,C5)].
}
  \item{In}{
Input carbon amount. It can be a scalar constant or a 2-column dataframe containing time dependent organic matter input. The two columns are time and carbon input
}
  \item{Dr}{
ratio of decomposable plant material (DPM) to resistant plant material (RPM). Default value is 1.44
}
  \item{effcts}{
a constant or dataframe of environmental effects on carbon decomposition rates. If it's a dataframe of time-dependent variables, then the length of the dataframe should be similar to the length of time (t) vector
}
  \item{solver}{
name of subroutines for solving first order odinary differential equations for organic matter decay in the soil. The subroutines are lsoda, lsodes,rk4, euler,lsode, lsodar,ode23, radau,etc. from deSolve
}
}
\details{
vector t can be years or months sequentially arranged with the start-time as the minimum and end-time as the maximum time. Initial carbon pools are also provided as a vector of five items: C1,C2,C3,C4,C5 in that order where C1 is the pool in the decomposable plant material (DPM) compartment, C2 is pool in the resistant plant material (RPM) compartment, C3 is the pool in the microbial biomass (BIO) compartment, C4 is the pool in the humified organic matter (HUM) compartment, and C5 is the pool in the inert organic matter (IOM) compartment.
}
\value{
nx6 matrix of carbon pools with time in the five compartments DPM, RPM, BIO, HUM, and IOM in that order (time, C1, C2, c3, C4, C5).
}
\references{
Coleman, K. and Jenkinson, D. 2014. ROTHC-26.3 A model for the turnover of carbon in soils: Model description and users guide (Windows version). Rothamsted Research Harpenden Herts AL5 2JQ

Jenkinson, D. S., Andrew, S. P. S., Lynch, J. M., Goss, M. J., Tinker, P. B. 1990. The Turnover of Organic Carbon and Nitrogen in Soil. Philosophical Transactions: Biological Sciences, 329: 361–368.
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{RotCmoistcorrection}}, \code{\link{NPPmodel}}
}
\examples{
library(deSolve)
Cin=c(0.6,0.1,0.3,0.1,2.7)
T=seq(1/12,200,by=1/12)
hw=carbonTurnover(tt=T,clay=23.4,C0=Cin,In=1.2,Dr=1.44,effcts=0.85,"euler")
matplot(T,hw[,2:6], type="l", lty=1, xlab="Time", ylab="C stocks (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),lty=1, col=1:5, bty="n")
}
