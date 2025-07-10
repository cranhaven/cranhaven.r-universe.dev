\name{Tani}
\alias{Tani}
\title{
A Tani water retention model
}
\description{
This is a three- parameter function to determine water retention hydraulic parameters contained in the Tani water retention model
}
\usage{
Tani(x, thr, ths, alp)
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
Tani model is a type of nonlinear curve fitting model for fitting water retention characteristics using experimental data. It requires initial parameter estimates for the model to work. This can be done using Dstart function (e.g. thr=Dstart(data)[1]). If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Tani M. 1982. The properties of water-table rise produced by a one-dimensional, vertical, unsaturated flow. Journal of Japan Forestry Society 64: 409-418
}
\author{
Christian Thine Omuto
}

\seealso{
\code{\link{Brook}}, \code{\link{Ruso}}, \code{\link{Campbel}}
}
\examples{
data(isric)
pf <- subset(isric, Sample=="Italy4")
library(minpack.lm)
tani.ns <- nlsLM(y ~ Tani(x,thr,ths,alp), data = pf,
                 control = nls.lm.control(maxiter=200),
                 start = c(thr=Dstart(pf)[1],ths=Dstart(pf)[2], alp=Dstart(pf)[3]))
coef(tani.ns)
}
\keyword{ models }
\keyword{ manip }
