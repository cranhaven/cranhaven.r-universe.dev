\name{MCF2004}
\alias{MCF2004}
\title{Body Mass Estimates Using Mazzetta et al. (2004)}
\description{This function is meant for quadrupedal mass estimates (in particularly sauropod dinosaurs) and offers some of the body mass prediction equations published by Mazzetta et al. (2004) derived from volumetric reconstructions of sauropod dinosaurs}
\usage{MCF2004(X, eqn, data = NULL)}
\arguments{
  \item{X}{a value or vector representing the variable in the equation (in mm)}
  \item{eqn}{a character specifying the desired prediction equation (see Details)}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
}
\details{Possible equations (\code{eqn =}) include: femur length (\code{"FL"}), modified femur length (\code{"FL*"}), femur circumference (\code{"FC"}), tibia length (\code{"TL"}), and tibia circumference (\code{"TC"}). See Mazzetta et al. (2004) for specific details and Campione & Evans (2020) for evaluations of the models.
If a data object is specified, the mass estiamtes will be added as a column to the data.frame or matrix within that object.}
\value{
	\item{MCF2004}{A numeric value or vector representing the mass estimate(s) in grams}
}
\author{Nicolas E. Campione}
\references{
	Mazzetta, G. V., Christiansen, P. and Farina, R. A. (2004) Giants and bizzares: body size of some southern South American Cretaceous dinosaurs. Historical Biology, 16, 71-83.
	
	Campione, N. E. and Evans, D. C. (2020) The accuracy and precisions of body mass estimation in non-avian dinosaurs. Biological Reviews.
}
\seealso{
	\code{\link{QE}}
  \code{\link{quadrupeds}}
  \code{\link{MR}}
	\code{\link{AHR1985}}
}
\examples{
##Quadrupedal dinosaurs
data(dinos)
MCF2004(dinos$FC, eqn = "FC", data = dinos)
}