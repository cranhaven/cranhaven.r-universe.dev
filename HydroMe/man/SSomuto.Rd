\name{SSomuto}
\alias{SSomuto}
\title{'Omuto' Water Retention Model}
\description{
This is a four- parameter function to determine water retention hydraulic parameters contained in a bimodal pore-size distribution developed by Omuto. The parameters are for the first and second compartments
}
\usage{
SSomuto(input, Ths1, alp1, Ths2, alp2)
}
\arguments{
  \item{input}{
Suction potential/head as contained in the x-column of the xy water retention table or data
}
  \item{Ths1}{
This is saturated moisture content in the first compartment of a bimodal pore-size distribution water retention characteristics
}
  \item{alp1}{
This is the inverse of air-entry potential in the first compartment of a bimodal pore-size distribution water retention characteristics
}
  \item{Ths2}{
This is saturated moisture content in the second compartment of a bimodal pore-size distribution water retention characteristics
}
  \item{alp2}{
This is the inverse of air-entry potential in the second compartment of a bimodal pore-size distribution water retention characteristics
}
}
\details{
Omuto model is a self-starting biexponential model for nonlinear curve fitting model to water retention data. If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
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
\note{
This function can also be used to fit water retention characteristics for a grouped dataset as well as with mixed-effects modelling
}

\seealso{
\code{\link{SSvgm}}, \code{\link{SSgard}}
}
\examples{
data(isric)
isric1 <- isric[1:32,]
library(nlme)
omuto <- nlsList(y ~ SSomuto(x,ths1,alp1,ths2,alp2) | Sample,  isric1)
omuto.nlme <- nlme(omuto)## for fitting mixed-effects models
summary(omuto.nlme)
}
\keyword{ models }

\keyword{ manip }
