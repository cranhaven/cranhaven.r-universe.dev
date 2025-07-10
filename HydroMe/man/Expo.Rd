\name{Expo}
\alias{Expo}
\title{
Exponential water retention model
}
\description{
This is a three- parameter function to determine water retention hydraulic parameters contained in the Exponential water retention model
}
\usage{
Expo(x, thr, ths, alp)
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
}
\details{
An exponential model is a type of nonlinear curve fitting model for fitting water retention characteristics using experimental data. It requires initial parameter estimates for the model to work. This can be obtained using Dstart function (e.g. thr=Dstart(data)[1]). If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Omuto CT. 2009. Biexponential model for water retention characteristics. Geoderma 149:235-242
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{SSomuto}}, \code{\link{SSfredlund}},\code{\link{SSgard}}
}
\examples{
##Attach sample data (isric data) from the package
data(isric)
pf <- subset(isric, Sample=="Benin2")
library(minpack.lm)
expo.ns <- nlsLM(y ~ Expo(x,thr,ths,alp), data=pf,
                 control = nls.lm.control(maxiter=200),
                 start = c(thr=Dstart(pf)[1],ths=Dstart(pf)[2], alp=Dstart(pf)[3]))
cor(fitted(expo.ns),pf$y)^2
}

\keyword{ models }
\keyword{ manip }
