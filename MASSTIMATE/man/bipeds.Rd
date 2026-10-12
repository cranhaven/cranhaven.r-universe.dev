\name{bipeds}
\alias{bipeds}
\title{Body Mass Estimates Using cQE, AHR1985, CF2004, and CM1992}
\description{This function returns body mass estimates for bipeds based on minimum femoral circumference using a set of five equations (intended for non-avian dinosaurs)}
\usage{bipeds(FC, cQE.eq = "raw", cQE.cor = 2, data = NULL)}
\arguments{
  \item{FC}{a value or vector representing femoral circumference (should be in mm)}
  \item{cQE.eq}{desired estimation equation for the cQE. Two possible choices (See \code{\link{cQE}}). \code{"raw"} is default}
  \item{cQE.cor}{correction factor to be used (\eqn{\alpha{^2}}{\alpha^2}). The default is 2, as per Campione et al. (2014)}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
}
\details{
This function returns estimates from five  equations intented for terrestrial bipeds (in particular non-avian dinosaurs): Campione et al. (2014); Campione (in review); Anderson et al. (1985); Christiansen and Farina (2004); Campbell and Marcus (1992). For more details please see the specific functions.

If a data object is specified, the mass estiamtes and additional results will be added as columns to the data.frame or matrix within that object.
}
\value{
Nine numeric values or columns are returned (all masses are in grams):
	\item{cQE}{A numeric value or vector of mass estimates using cQE}
	\item{upper.cQE}{A numeric value or vector representing the upper prediction error of the cQE based on that derived for the specific equation in Campione and Evans (2012)}
	\item{lower.cQE}{A numeric value or vector representing the lower prediction error of the cQE}
	\item{qcQE}{A numeric value or vector of mass estimates using qcQE (quadratic equation)}
	\item{upper.qcQE}{A numeric value or vector representing the upper prediction error of qcQE derived for the specific equation in Campione (in review)}
	\item{lower.qcQE}{A numeric value or vector representing the lower prediction error of qcQE}
  \item{AHR1985}{A numeric value or vector of mass estimates using Anderson et al. (1985)}
  \item{CF2004}{A numeric value or vector of mass estimates using Christiansen and Farina (2004)}
  \item{CM1992}{A numeric value or vector of mass estimates using Campbell and Marcus (1992)}
}
\author{Nicolas E. Campione}
\references{
  Anderson, J. F., Hall-Martin, A. and Russell, D.A. (1985) Long-bone circumference and weight in mammals, birds and dinosaurs. Journal of the Zoological Society of London A, 207, 53-61.
  
  Campbell Jr., K.E. and Marcus, L. (1992) The relationships of hindlimb bone dimensions to body weight in birds. Natural History Museum of Los Angeles County Science Series, 36, 395-412.
  
	Campione, N. E., Evans, D. C., Brown, C. M. and Carrano, M. T. (2014) Body mass estimation in non-avian bipeds using a theoretical conversion to quadruped stylopodial proportions. Methods in Ecology and Evolution, 5(9), 913-923.

  Campione, N. E. (2017) Extrapolation of a universal scaling relationship for estimating body masses in extinct terrestrial vertebrates. Paleobiology, 43, 693-699.
  
  Christiansen, P. and Farina, R. A. (2004) Mass prediction in theropod dinosaurs. Historical Biology, 16, 85-92.
}
\seealso{
  \code{\link{cQE}}
  \code{\link{AHR1985}}
  \code{\link{CF2004}}
	\code{\link{CM1992}}
}