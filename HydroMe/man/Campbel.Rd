\name{Campbel}
\alias{Campbel}
\title{
Campbell water retention model
}
\description{
This is a three-parameter function to determine water retention hydraulic parameters contained in the Campbell water retention model
}
\usage{
Campbel(x, ths, alp, nscal)
}
\arguments{
  \item{x}{
Suction potential/head as contained in the x-column of the xy water retention table or data
}
  \item{ths}{
This is the saturated moisture content. It's the moisture content when suction potential is very low (almost at the saturation point)
}
  \item{alp}{
It's the inverse of air-entry potential or bubbling pressure
}
  \item{nscal}{
This is a parameter or index for the pore-size distribution. nscal=1/b, where b is empirical parameter in the original Campbel model
}
}
\details{
A Campbell model is a type of nonlinear curve fitting model for fitting water retention characteristics using experimental data. It requires initial parameter estimates for the model to work. This can be obtained using Dstart function (e.g. thr=Dstart(data)[1]). If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Campbell GS. 1974. A simple method for determining unsaturated conductivity from
moisture retention data. Soil Science 117: 311-314
}
\author{
Christian Thine Omuto
}
\note{
Warnings: Choice of starting values for Campbel function parameter estimation needs a careful thought. Sometimes it may be necessary to use alp=1/Dstart(data)[3] or Dstart(data)[3] and nscal=Dstart(data)[4]-1 or nscal=Dstart(data)[4]. One has to try the starting variables that will work for the given data
}
\seealso{
\code{\link{Brook}}, \code{\link{SSvgm}}, \code{\link{SSfredlund}}
}
\examples{
data(isric)
pf <- subset(isric, Sample=="Nicaragua38")
library(minpack.lm)
campbel.ns <- nlsLM(y ~ Campbel(x,ths,alp,nscal), data=pf,
                  control = nls.lm.control(maxiter=200),
                  start = c(ths = Dstart(pf)[2],
                            alp=Dstart(pf)[3], nscal=-1*(Dstart(pf)[4]-1)))
plot(fitted(campbel.ns),pf$y)
coef(campbel.ns)
}
\keyword{ models }
\keyword{ manip }
