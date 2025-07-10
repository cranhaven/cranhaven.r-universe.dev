\name{Ruso}
\alias{Ruso}
\title{
A Russo  water retention model
}
\description{
This is a four- parameter function to determine water retention hydraulic parameters contained in the Russo water retention model
}
\usage{
Ruso(x, thr, ths, alp, nscal)
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
}
\details{
A Russo model is a type of nonlinear curve fitting model for fitting water retention characteristics using experimental data. It requires initial parameter estimates for the model to work. This can be obtained using Dstart function (e.g. thr=Dstart(data)[1]). If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Russo D. 1988. Determining soil hydraulic properties by parameter estimation: on the selection of a model for the hydraulic properties. Water Resources Research 24(3): 453-459
}
\author{
Christian Thine Omuto
}

\seealso{
\code{\link{Brook}}, \code{\link{Tani}}, \code{\link{Campbel}}
}
\examples{
## Attach sample data (isric data) from the package
data(isric)
pf <- subset(isric, Sample=="Italy5")
library(minpack.lm)
ruso.ns <- nlsLM(y ~ Ruso(x,thr,ths,alp,nscal), data=pf,
                 control = nls.lm.control(maxiter=200),
                 start = c(thr= Dstart(pf)[1], ths = Dstart(pf)[2],
                           alp= Dstart(pf)[3], nscal=Dstart(pf)[4]))
coef(ruso.ns)
}
\keyword{ models }
\keyword{ manip }
