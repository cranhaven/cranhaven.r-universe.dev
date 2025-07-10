\name{SSfredlund}
\alias{SSfredlund}
\title{
A Fredlund-Xing  water retention model
}
\description{
This is a five- parameter function to determine water retention hydraulic parameters contained in the Fredlund-Xing water retention model
}
\usage{
SSfredlund(x, thr, ths, alp, nscal, mscal)
}
\arguments{
  \item{x}{
Suction potential/head as contained in the x-column of the xy water retention table or data
}
  \item{thr}{
This is the residual moisture content. It's the moisture content when suction potential is very high (almost at the drying point)
}
  \item{ths}{
This is the saturated moisture content. It's the moisture content when suction potential is very low (almost at the saturation point)
}
  \item{alp}{
It's the inverse of air-entry potential or bubbling pressure
}
  \item{nscal}{
This is a parameter or index for the pore-size distribution
}
  \item{mscal}{
This is a scaling parameter which is also related to the index for the pore-size distribution
}
}
\details{
A Fredlund-Xing model is a self-starting type of nonlinear curve fitting model for fitting water retention characteristics using experimental data. If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Fredlund DG and Xing A. 1994. Equations for the soil-water characteristic curve. Canadian Geotechnical Journal 31: 521-532
}
\author{
Christian Thine Omuto
}

\seealso{
\code{\link{SSomuto}}, \code{\link{SSvgm}}, \code{\link{SSkosugi}}
}
\examples{
## Attach sample data (isric data) from the package
data(isric)
pf <- subset(isric, Sample=="Benin2")
library(minpack.lm)
suppressWarnings(
fredlund.ns <- nlsLM(y ~ SSfredlund(x,thr,ths,alp,nscal,mscal), data=pf,
                     control = nls.lm.control(maxiter=200)))
coef(fredlund.ns)
plot(fitted(fredlund.ns)~pf$y)
}
\keyword{ models }
\keyword{ manip }
