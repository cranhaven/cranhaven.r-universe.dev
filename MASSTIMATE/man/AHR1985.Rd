\name{AHR1985}
\alias{AHR1985}
\title{Body Mass Estimates Using Anderson et al. (1985)}
\description{This function estimates body mass based on humeral and/or femoral circumferences using the quadrupedal and bipedal equations of Anderson et al. (1985)}
\usage{AHR1985(HC = NULL, FC, equation = c("bip", "quad"), data=NULL)}
\arguments{
  \item{HC}{an optional value or vector representing humeral circumference (in mm)}
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{equation}{desired estimation equation. Two possible choices: \code{"quad"}, \code{"bip"}}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
}
\details{If \code{equation = "bip"} then only femoral circumference is needed. Both humeral and femoral circumference are needed to estimate body mass of a quadruped (\code{equation = "quad"}). If a data object is specified, the mass estiamtes will be added as a column to the data.frame or matrix within that object.}
\value{
	\item{AHR1985}{A numeric value or vector representing the mass estimate(s) in grams}
}
\author{Nicolas E. Campione}
\references{
	Anderson, J. F., Hall-Martin, A. and Russell, D.A. (1985) Long-bone circumference and weight in mammals, birds and dinosaurs. Journal of the Zoological Society of London A, 207, 53-61.
}
\seealso{
	\code{\link{QE}}
  \code{\link{cQE}}
  \code{\link{quadrupeds}}
  \code{\link{bipeds}}
	\code{\link{CM1992}}
  \code{\link{CF2004}}
  \code{\link{MCF2004}}
}
\examples{
##Quadrupedal dinosaurs
data(dinos)
AHR1985(dinos$HC, dinos$FC, equation = "quad", data = dinos)

##Bipedal dinosaurs
data(dinosbip)
AHR1985(HC=NULL, dinosbip$FC, equation = "bip", data = dinosbip)
}