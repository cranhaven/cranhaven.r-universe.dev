\name{quadrupeds}
\alias{quadrupeds}
\title{Body Mass Estimates Using QE, MR, AHR1985, and MCF2004}
\description{This function returns body mass estimates for quadrupeds using humeral and/or femoral circumferences (intended for non-avian dinosaurs)}
\usage{quadrupeds(HC, FC, QE_MR.eq = "raw", data = NULL)}
\arguments{
  \item{HC}{a value or vector representing humeral circumference (in mm)}
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{QE_MR.eq}{desired estimation equations to used by QE and MR. Two possible choices. \code{"raw"} is default}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
}
\details{
This function returns estimates from five equations intented for terrestrial quadrupeds (in particular non-avian dinosaurs): two from Campione and Evans (2012) based on bivariate and multiple regression approaches; a quadratic view of the bivariate regression (Campione in review); Anderson et al. (1985); Mazzetta et al. (2004). For more details please see the specific functions.

If a data object is specified, the mass estiamtes and additional results will be added as columns to the data.frame or matrix within that object.
}
\value{
11 numeric values or columns are returned (all masses are in grams):
	\item{QE}{A numeric value or vector of mass estimates using QE (bivariate regression)}
	\item{upper.QE}{A numeric value or vector representing the upper prediction error of the QE based on that derived for the specific equation by Campione and Evans (2012)}
	\item{lower.QE}{A numeric value or vector representing the lower prediction error of the QE}
	\item{qQE}{A numeric value or vector of mass estimates using qQE (quadratic equation)}
	\item{upper.qQE}{A numeric value or vector representing the upper prediction error of qQE derived for the specific equation in Campione (in review)}
	\item{lower.qQE}{A numeric value or vector representing the lower prediction error of qQE}
  \item{MR}{A numeric value or vector of mass estimates using MR (multiple regression)}
	\item{upper.MR}{A numeric value or vector representing the upper prediction error of the MR based on that derived for the specific equation by Campione and Evans (2012)}
	\item{lower.MR}{A numeric value or vector representing the lower prediction error of the MR}
  \item{AHR1985}{A numeric value or vector of mass estimates using Anderson et al. (1985)}
  \item{MCF2004}{A numeric value or vector of mass estimates using Mazzetta et al. (2004)}
}
\author{Nicolas E. Campione}
\references{
  Anderson, J. F., Hall-Martin, A. and Russell, D. A. (1985) Long-bone circumference and weight in mammals, birds and dinosaurs. Journal of the Zoological Society of London A, 207, 53-61.
  
	Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
	
  Campione, N. E. (2017) Extrapolation of a universal scaling relationship for estimating body masses in extinct terrestrial vertebrates. Paleobiology, 43, 693-699.
  
  Mazzetta, G. V., Christiansen, P. and Farina, R. A. (2004) Giants and bizzares: body size of some southern South American Cretaceous dinosaurs. Historical Biology, 16, 71-83.
}
\seealso{
  \code{\link{QE}}
  \code{\link{MR}}
  \code{\link{AHR1985}}
  \code{\link{MCF2004}}
}