\name{SSvgm4}
\alias{SSvgm4}
\title{
A four-parameter van Genuchten water retention model
}
\description{
This is a four-parameter function to determine water retention hydraulic parameters contained in the van Genuchten water retention model
}
\usage{
SSvgm4(input, Thr, Ths, alp, nscal)
}
\arguments{
  \item{input}{
Suction potential/head as contained in the x-column of the xy water retention table or data
}
  \item{Thr}{
This is the residual moisture content. It's the moisture content when suction potential is very high (almost at the drying point)
}
  \item{Ths}{
This is the saturated moisture content. It's the moisture content when suction potential is very low (almost at the saturation point)
}
  \item{alp}{
It's natural logarithm of the inverse of air-entry potential or bubbling pressure
}
  \item{nscal}{
This is a parameter or index for the pore-size distribution
}
}
\details{
Van Genuchten model is a type of self-starting nonlinear curve fitting model for fitting water retention characteristics using experimental data. In this model, the restriction of m=1-1/n has been imposed on the water retention characteristics. Air-entry potential = exp(alp). If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
van Genuchten MT. 1980. A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil Science Society of America Journal 44: 892-898
}
\author{
Christian Thine Omuto
}

\seealso{
\code{\link{SSomuto}}, \code{\link{Brook}}, \code{\link{Campbel}}
}
\examples{
data(isric)
library(nlme)
library(minpack.lm)
pfdata=subset(isric,Country=="Benin")
suppressWarnings(
vn4.ns <- nlsList(y ~ SSvgm4(x,thr,ths,alp,nscal)|Sample,
               data = pfdata,
               control = nls.lm.control(maxiter=200)))
coef(vn4.ns)
plot(fitted(vn4.ns)~pfdata$y)
cor(fitted(vn4.ns),pfdata$y)^2
}
\keyword{ models }
\keyword{ manip}
