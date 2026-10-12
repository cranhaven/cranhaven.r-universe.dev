\name{cQE}
\alias{cQE}
\title{Body Mass Estimates Using Bipedal Correction Factor}
\description{This function presents equations from Campione et al. (2014) for esimating body mass in bipeds using minimum femoral circumference and based on a correction of the quadrupedal equations from Campione and Evans (2012)}
\usage{cQE(FC, equation = "raw", cor = 2, quadratic = FALSE, 
data = NULL, return_PI = FALSE)}
\arguments{
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{equation}{desired estimation equation. Two possible choices (See Details)}
  \item{cor}{correction factor to be used (\eqn{\alpha{^2}}{\alpha^2}). The default is 2, as per Campione et al. (2014)}
  \item{quadratic}{a logical indicating whether estimates based on the quadratic equation should be used (See Details)}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
  \item{return_PI}{an optional logical value to also provide the wider prediction intervals on the estimate, currenly only works if \code{equation = "raw"}}
}
\details{
The function includes two different equations. \code{equation = "raw"} (default) applies the correction factor derived in Campione et al. (2014) to the raw (non-phylogenetically corrected) bivariate regression equation from Campione and Evans (2012). \code{equation = "phylocor"} applies the same correction factor to the phylogenetically corrected equation presented in the same study.

\code{cor = 2} refers to the correction factor (\eqn{\alpha{^2}}{\alpha^2}) to be used. The default (\code{cor = 2}) refers the initial derivation in Campione et al. (2014), however, this value can be modified based on the level of eccentricity of the femur. A set of values for \code{cor} corresponding to set eccentricities can be found in table S2 of Campione et al. (2014).

If \code{quadratic = TRUE}, then a second set of estimates will be returned based on a quadratic estimation equation (Campione, 2017).

If a data object is specified, the mass estiamtes and additional results will be added as columns to the data.frame or matrix within that object.
}
\value{
Eight numeric values or columns are returned if \code{quadratic = TRUE} (identified by \code{q}):
  \item{log.cQE}{A numeric value or vector representing the mass estimate(s) in log10 grams}
	\item{cQE}{A numeric value or vector representing the mass estimate(s) in grams}
	\item{lower.cQE}{A numeric value or vector representing the lower prediction error derived for the specific equation in Campione and Evans (2012)}
	\item{upper.cQE}{A numeric value or vector representing the upper prediction error}
	\item{log.qcQE}{A numeric value or vector representing the mass estimate(s) in log10 grams using a quadratic equation}
	\item{qcQE}{A numeric value or vector representing the mass estimate(s) in grams}
	\item{lower.qcQE}{A numeric value or vector representing the lower prediction error derived for the specific equation by Campione (2017)}
	\item{upper.qcQE}{A numeric value or vector representing the upper prediction error}
}
\author{Nicolas E. Campione}
\references{
	Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
  
	Campione, N. E., Evans, D. C., Brown, C. M. and Carrano, M. T. (2014) Body mass estimation in non-avian bipeds using a theoretical conversion to quadruped stylopodial proportions. Methods in Ecology and Evolution, 5(9), 913-923.
	
  Campione, N. E. (2017) Extrapolation of a universal scaling relationship for estimating body masses in extinct terrestrial vertebrates. Paleobiology, 43 (4), 693-699.
}
\seealso{
  \code{\link{QE}}
  \code{\link{bipeds}}
  \code{\link{AHR1985}}
  \code{\link{CF2004}}
	\code{\link{CM1992}}
}
\examples{
##Bipedal dinosaurs
data(dinosbip)

#Estimates for Tyrannosaurus (FMNH PR 2081 "Sue")
sue<-which(dinosbip$Taxon=="TyrannosaurusFMNH2081")
cQE(dinosbip$FC[sue]) #default correction factor
cQE(dinosbip$FC[sue], cor = 1.815) #based on eccentricity of the femur

##Estimates of bipedal dinosaurs using phylogeneteically corrected linear and quadratic equations
cQE(dinosbip$FC, equation = "phylocor", cor = dinosbip$cor, quadratic = TRUE, data = dinosbip)
}