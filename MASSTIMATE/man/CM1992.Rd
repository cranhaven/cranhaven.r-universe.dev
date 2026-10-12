\name{CM1992}
\alias{CM1992}
\title{Body Mass Estimates Using Campbell and Marcus (1992)}
\description{This function estimates body mass based on femoral circumference using the avian equation of Campbell and Marcus (1992)}
\usage{CM1992(FC, data = NULL)}
\arguments{
  \item{FC}{a value of vector representing femoral circumference (in mm)}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
}
\details{If a data object is specified, the mass estiamtes will be added as a column to the data.frame or matrix within that object.}
\value{
	\item{CM1992}{A numeric value or vector representing the mass estimate(s) in grams}
}
\author{Nicolas E. Campione}
\references{
	Campbell Jr., K.E. and Marcus, L. (1992) The relationships of hindlimb bone dimensions to body weight in birds. Natural History Museum of Los Angeles County Science Series, 36, 395-412.
}
\seealso{
	\code{\link{cQE}}
  \code{\link{bipeds}}
	\code{\link{AHR1985}}
  \code{\link{CF2004}}
}
\examples{
##Bipedal dinosaurs
data(dinosbip)
CM1992(dinosbip$FC, data = dinosbip)
}