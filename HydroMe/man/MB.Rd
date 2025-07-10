\name{MB}
\alias{MB}
\title{
McKee and Bumb water retention model
}
\description{
This is a three- parameter function to determine water retention hydraulic parameters contained in the McKee and Bumb water retention model
}
\usage{
MB(x,thr,ths,alp)
}
\arguments{
  \item{x}{
Suction potential/head as contained in the x-column of the xy water retention table or data
}
  \item{thr}{
This is the residual moisture content.It's the moisture content when suction potential is very high (almost at the drying point)
}
  \item{ths}{
This is the saturated moisture content. It's the moisture content when suction potential is very low (almost at the saturation point)
}
  \item{alp}{
This is the inverse of air-entry potential or bubbling pressure
}
}
\details{
McKee and Bumb model is a type of nonlinear curve fitting model for fitting water retention characteristics using experimental data. It requires initial parameter estimates for the model to work. This can be obtained using Dstart function (e.g. thr=Dstart(data)[1]). If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
McKee CR and Bumb AC.1987. Flow-testing coalbed methane production wells in presence of water and gas. SPE Formation Evaluation,vol. 2, no. 4, pp. 599--608
}
\author{
Christian Thine Omuto
}
\note{
Warnings: Choice of starting values for SSMB function parameter estimation needs a careful thought. Sometimes it may be necessary to use alp=1/Dstart(data)[3] or Dstart(data)[3]. One has to try the starting variables that will work for the given data.
}
\seealso{
\code{\link{SSomuto}}, \code{\link{Expo}}, \code{\link{Campbel}}
}
\examples{
##Attach sample data (isric data) from the package
data(isric)
library(minpack.lm)
pf <- subset(isric, Sample=="Italy5")
mb.ns=nlsLM(y~MB(x,thr,ths,alp),data=pf, control=nls.lm.control(maxiter=200),
            start=c(thr=Dstart(pf)[1],ths=Dstart(pf)[2], alp=Dstart(pf)[3]))
summary(mb.ns)
coef(mb.ns)
}
\keyword{ Models }
\keyword{ manip }
