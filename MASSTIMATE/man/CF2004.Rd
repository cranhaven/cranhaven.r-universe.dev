\name{CF2004}
\alias{CF2004}
\title{Body Mass Estimates Using Christiansen and Farina (2004)}
\description{This function is meant for bipedal mass estimate (in particular theropod dinosaurs) and offers some of the body mass prediction equations published by Christiansen and Farina (2004) derived from volumetric reconstructions of theropod dinosaurs}
\usage{CF2004(X, X2 = NULL, eqn, data = NULL)}
\arguments{
  \item{X}{a value or vector representing the first variable in the equation (in mm)}
  \item{X2}{an optional second variable with same length as \code{X}, should the equation be multiple (see Details) {in mm}}
  \item{eqn}{a character specifying the desired prediction equation (see Details)}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
}
\details{Possible bivariate regression equations (\code{eqn =}) include: femur length (\code{"FL"}), femur circumference (\code{"FC"}), femur anteroposterior diameter (\code{"FAP"}), femur mediolateral diameter (\code{"FML"}), tibia circumference (\code{"TC"}), tibia distal mediolateral diameter (\code{"TdistML"}), fibula distal anteroposterior diameter (\code{"FidistAP"}). Multiple regression equations include femur circumference + length (\code{"FC+FL"}), femur mediolateral diameter + femur length (\code{"FML+FL"}), tibia circumference + femur length (\code{"TC+FL"}), tibia length + femur circumference (\code{"TL+FC"}), and tibia circumference + femur anteroposterior diameter (\code{"TC+FAP"}). If a multiple regression equation is chosen, \code{X2 != NULL}. See Christiansen & Farina (2004) for specific details and Campione & Evans (2020) for evaluations of the models.
If a data object is specified, the mass estiamtes will be added as a column to the data.frame or matrix within that object.}
\value{
	\item{CF2004}{A vector representing the mass estimate(s) in log grams and grams}
}
\author{Nicolas E. Campione}
\references{
	Christiansen, P. and Farina, R. A. (2004) Mass prediction in theropod dinosaurs. Historical Biology, 16, 85-92.
	
	Campione, N. E. and Evans, D. C. (2020) The accuracy and precisions of body mass estimation in non-avian dinosaurs. Biological Reviews.
}
\seealso{
	\code{\link{cQE}}
  \code{\link{bipeds}}
	\code{\link{AHR1985}}
  \code{\link{CM1992}}
}
\examples{
##Bipedal dinosaurs
data(dinosbip)
CF2004(dinosbip$FC, eqn = "FC", data = dinosbip)
}